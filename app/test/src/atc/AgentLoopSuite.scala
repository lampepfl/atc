package atc

import atc.agent.*
import atc.config.Config
import atc.llm.*
import atc.perms.{Decision, ExecRequest}
import atc.sandbox.ReplSession

import java.nio.file.Files

/** The agent loop with the Scala tool runner, driven by a scripted model (resume,
  * tool budget, multi-tool, cancellation, refusal, compaction, usage accounting).
  * The REPL starts only in the tests that run Scala. */
class AgentLoopSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  /** An agent on a fresh [[TestEnv]], with a REPL session that starts on first use. */
  private final class Loop(model: ChatModel, cfg: Config = Config(), commands: List[String] = List("echo")):
    val env: TestEnv = TestEnv(commands = commands, prefix = "atc-loop")
    lazy val session: ReplSession = env.newSession()
    val ui: RecordingUI = RecordingUI()
    val agent: Agent = Agent(cfg, AgentEnvironment.current(env.root), env.policy, ui, model, None, None)
    def turn(input: String, cancelled: () => Boolean = () => false): TurnOutcome =
      agent.turn(session, input, cancelled)
    def assistants: List[Msg.Assistant] = agent.history.collect { case m: Msg.Assistant => m }
    def toolResults: List[Msg.ToolResults] = agent.history.collect { case m: Msg.ToolResults => m }

  /** A loop whose model `model` makes from the estimated size of the next request
    * after `history`, measured on the same agent before that model is chosen. */
  private def sized(cfg: Config, history: Msg*)(model: Long => ScriptedModel): (Loop, ScriptedModel) =
    val loop = Loop(ScriptedModel("m", Nil), cfg)
    val chosen = model(loop.agent.contextUsage.tokens + history.map(ContextManager.estimateTokens(_)).sum)
    loop.agent.model = chosen
    (loop, chosen)

  test("a sandbox restart is announced to the model on the next turn, once"):
    // A mode switch or /reset replaces the REPL, so the agent's `val`s and `def`s
    // are gone. Without a notice the model concludes that the documented
    // "definitions persist" guarantee is broken and wastes a round on it.
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Reply("a"), ScriptedModel.Reply("b"))))
    import loop.*
    agent.noteSandboxRestarted("the sandbox mode changed to read-only")
    turn("what is left?")
    val first = agent.history.head.asInstanceOf[Msg.User].text
    assert(first.contains("[sandbox notice]"), first)
    assert(first.contains("read-only"), first)
    assert(first.endsWith("what is left?"), first)
    // The notice is not repeated on later turns.
    turn("and now?")
    val second = agent.history.collect { case u: Msg.User => u }.last.text
    assertEquals(second, "and now?")

  test("code the user ran (/run) is reported to the model on the next turn, after a restart notice, once"):
    // The REPL is shared: the user's definitions now exist in the session the
    // model continues in, so it must hear what was run and what came of it.
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Reply("a"), ScriptedModel.Reply("b"))))
    import loop.*
    agent.noteSandboxRestarted("you asked for /reset")
    agent.noteUserRan("val answer = 42", session.run("val answer = 42"), Nil)
    turn("double it")
    val first = agent.history.head.asInstanceOf[Msg.User].text
    val (restart, ran) = (first.indexOf("[sandbox notice]"), first.indexOf("[user ran code]"))
    assert(restart >= 0 && ran > restart, first)
    assert(first.contains("```scala\nval answer = 42\n```"), first)
    assert(first.contains("answer: Int = 42"), first)
    assert(first.endsWith("double it"), first)
    turn("and now?")
    assertEquals(agent.history.collect { case u: Msg.User => u }.last.text, "and now?")

  test("clear() drops a pending restart notice"):
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Reply("a"))))
    import loop.*
    agent.noteSandboxRestarted("whatever")
    agent.clear()
    turn("fresh")
    assertEquals(agent.history.head, Msg.User("fresh"))

  test("a plain reply becomes one assistant message and streams its text"):
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Reply("hello there"))))
    import loop.*
    turn("hi")
    assertEquals(agent.history.head, Msg.User("hi"))
    assertEquals(agent.history.last, Msg.Assistant("hello there", Nil, None))
    assert(ui.deltas.contains("hello there"))
    assertEquals(ui.ends, 1)

  test("an empty final model response is made visible and keeps neutral history valid"):
    val native = NativeTurn("scripted", "m", "empty-native")
    val empty = completion("", native = Some(native))
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Comp(empty), ScriptedModel.Reply("recovered"))))
    import loop.*
    turn("first")
    agent.history.last match
      case Msg.Assistant(text, calls, replay) =>
        assert(text.contains("model returned no response"), text)
        assertEquals(calls, Nil)
        assertEquals(replay, None)
      case other => fail(s"expected assistant marker, got $other")
    assert(ui.warnings.exists(_.contains("returned no response")), ui.warnings.toString)
    turn("second")
    assertEquals(agent.history.last, Msg.Assistant("recovered", Nil, None))

  test("a tool call runs, its result is appended, then the model answers"):
    val model = ScriptedModel(
      "m",
      Seq(
        ScriptedModel.tool("""println("side effect"); 21 * 2"""),
        ScriptedModel.Reply("the answer is 42"),
      )
    )
    val loop = Loop(model)
    import loop.*
    turn("compute")
    val trs = toolResults
    assertEquals(trs.size, 1)
    assertEquals(trs.head.results.size, 1)
    assert(!trs.head.results.head.isError, trs.toString)
    assert(trs.head.results.head.output.contains("42"), trs.head.results.head.output)
    assert(env.agentOut.toString.contains("side effect"))
    assertEquals(ui.toolStarts.size, 1)
    assertEquals(ui.toolEnds.toList, List(true))
    assertEquals(agent.history.last, Msg.Assistant("the answer is 42", Nil, None))
    // the model saw the tool result before its final reply: it was in the history
    // passed to the second complete() call.
    assert(model.seenHistories(1).exists(_.isInstanceOf[Msg.ToolResults]))

  test("a failing tool call is reported as an error result"):
    val loop = Loop(ScriptedModel(
      "m",
      Seq(
        ScriptedModel.tool("""throw new RuntimeException("boom")"""),
        ScriptedModel.Reply("noted"),
      )
    ))
    import loop.*
    turn("go")
    val tr = toolResults.head.results.head
    assert(tr.isError)
    assert(tr.output.contains("boom"), tr.output)
    assertEquals(ui.toolEnds.toList, List(false))

  test("several tool calls in one completion all run, in order"):
    val loop = Loop(ScriptedModel(
      "m",
      Seq(
        ScriptedModel.tools("val a = 1", "val b = a + 1", "println(b)"),
        ScriptedModel.Reply("done"),
      )
    ))
    import loop.*
    turn("go")
    val results = toolResults.head.results
    assertEquals(results.size, 3)
    assert(results.forall(!_.isError), results.toString)
    assert(results.last.output.contains("2"), results.last.output)

  test("empty / missing code argument is rejected without touching the REPL"):
    val missing = completion("x", List(ToolCall("c1", Prompts.ToolName, "{}")), reason = "tool_use")
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Comp(missing), ScriptedModel.Reply("ok"))))
    import loop.*
    turn("go")
    val tr = toolResults.head.results.head
    assert(tr.isError)
    assert(tr.output.contains("Missing 'code'"), tr.output)
    assert(env.session.isEmpty, "the REPL was started")

  test("an unknown tool name is reported back to the model"):
    val unknown = completion("x", List(ToolCall("c1", "delete_everything", "{}")), reason = "tool_use")
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Comp(unknown), ScriptedModel.Reply("ok"))))
    import loop.*
    turn("go")
    val tr = toolResults.head.results.head
    assert(tr.isError)
    assert(tr.output.contains("Unknown tool"), tr.output)
    assert(tr.output.contains(Prompts.ToolName), tr.output)

  test("the per-turn tool budget stops extra calls with an explanatory result"):
    val loop = Loop(
      ScriptedModel(
        "m",
        Seq(
          ScriptedModel.tool("1 + 1"),
          ScriptedModel.tool("2 + 2"),
          ScriptedModel.Reply("stopping"),
        )
      ),
      Config(maxToolCalls = 1)
    )
    import loop.*
    turn("go")
    val allResults = toolResults.flatMap(_.results)
    assertEquals(allResults.size, 2)
    assert(!allResults.head.isError)
    assert(allResults(1).isError)
    assert(allResults(1).output.contains("budget"), allResults(1).output)
    assertEquals(agent.toolCalls, 1) // only the first actually ran

  test("the tool budget is a checkpoint: when the UI agrees, the turn continues for another budget"):
    val steps = Seq(
      ScriptedModel.tool("1 + 1"),
      ScriptedModel.tool("2 + 2"),
      ScriptedModel.tool("3 + 3"),
      ScriptedModel.Reply("done"),
    )
    val extended = Loop(ScriptedModel("m", steps), Config(maxToolCalls = 1))
    extended.ui.allowMoreToolCalls = true
    extended.turn("go")
    assertEquals(extended.agent.toolCalls, 3) // every call ran
    assertEquals(extended.ui.budgetAsks, 2) // asked before the 2nd and the 3rd
    assert(extended.toolResults.flatMap(_.results).forall(!_.isError))
    // a budget of 0 means "no tools" and is never extended
    val none =
      Loop(ScriptedModel("m", Seq(ScriptedModel.tool("1"), ScriptedModel.Reply("x"))), Config(maxToolCalls = 0))
    none.ui.allowMoreToolCalls = true
    none.turn("go")
    assertEquals(none.agent.toolCalls, 0)
    assertEquals(none.ui.budgetAsks, 0)

  test("a model cannot loop forever after exhausting the tool budget"):
    val steps = Seq.fill(10)(ScriptedModel.tool("1 + 1"))
    val loop = Loop(ScriptedModel("m", steps), Config(maxToolCalls = 0))
    import loop.*
    turn("go")
    assertEquals(toolResults.size, Agent.MaxBudgetRejections)
    assertEquals(agent.toolCalls, 0)
    assert(ui.warnings.exists(_.contains("tool budget")))

  test("declining the tool budget asks once per turn, not once per queued call"):
    // A batch of calls past the budget prompts once, not once per call, and the
    // refusal holds for the later rounds of the turn.
    val steps = Seq(
      ScriptedModel.tool("1 + 1"),
      ScriptedModel.tools("2 + 2", "3 + 3", "4 + 4"), // three calls, all over budget
      ScriptedModel.tool("5 + 5"), // and the model keeps asking
      ScriptedModel.Reply("done"),
    )
    val loop = Loop(ScriptedModel("m", steps), Config(maxToolCalls = 1))
    import loop.*
    ui.allowMoreToolCalls = false
    turn("go")
    assertEquals(agent.toolCalls, 1) // only the first call ran
    assertEquals(ui.budgetAsks, 1) // one decline covers the whole turn
    val errs = toolResults.flatMap(_.results).filter(_.isError)
    assertEquals(errs.size, 4) // the remaining four calls all got the budget result
    assert(errs.forall(_.output.contains("budget")), errs.toString)

  test("a failed turn appends an assistant marker, keeping the history well-formed"):
    // Without the marker the next turn would stack two user messages in a row,
    // which providers (Anthropic) reject.
    val loop =
      Loop(ScriptedModel("m", Seq(ScriptedModel.Throw(RuntimeException("api down")), ScriptedModel.Reply("recovered"))))
    import loop.*
    intercept[RuntimeException](turn("first"))
    assertEquals(ui.ends, 1) // a failed stream still closes the assistant UI block
    assertEquals(agent.history.last, Msg.Assistant("[turn failed: api down]", Nil, None))
    turn("second")
    val users = agent.history.collect { case u: Msg.User => u.text }
    assertEquals(users, List("first", "second")) // no consecutive user messages
    assertEquals(agent.history.last, Msg.Assistant("recovered", Nil, None))

  test("a provider failure after an internal truncation continuation repairs the user-role ending"):
    val cut = completion("partial", reason = "max_tokens", stop = CompletionStop.Truncated)
    val loop = Loop(ScriptedModel(
      "m",
      Seq(ScriptedModel.Comp(cut), ScriptedModel.Throw(RuntimeException("resume failed")), ScriptedModel.Reply("ok"))
    ))
    import loop.*
    intercept[RuntimeException](turn("first"))
    assert(agent.history.exists(_.isInstanceOf[Msg.Continuation]), agent.history.toString)
    assertEquals(agent.history.last, Msg.Assistant("[turn failed: resume failed]", Nil, None))
    turn("second")
    assertEquals(agent.history.last, Msg.Assistant("ok", Nil, None))

  test("an unfinished completion is resumed"):
    val loop = Loop(ScriptedModel(
      "m",
      Seq(
        ScriptedModel.unfinished("searching the web…"),
        ScriptedModel.Reply("Here is what I found."),
      )
    ))
    import loop.*
    turn("look it up")
    assert(!agent.history.exists {
      case Msg.User(t) => t != "look it up"; case _ => false
    }) // no injected user messages
    assertEquals(assistants.map(_.text), List("searching the web…", "Here is what I found."))
    assert(ui.statuses.exists(_.contains("resuming")))

  test("resuming is bounded by MaxResumes"):
    val steps = Seq.fill(Agent.MaxResumes + 3)(ScriptedModel.unfinished("still going"))
    val loop = Loop(ScriptedModel("m", steps))
    import loop.*
    turn("go")
    // MaxResumes resume-rounds after the first completion → MaxResumes+1 assistant messages, then stop
    assertEquals(assistants.size, Agent.MaxResumes + 1)
    assert(ui.warnings.exists(_.contains("remained unfinished")), ui.warnings.toString)

  test("provider length stops resume instead of being mistaken for a final answer"):
    val cut = completion("partial", reason = "length", stop = CompletionStop.Truncated)
    val model = ScriptedModel("m", Seq(ScriptedModel.Comp(cut), ScriptedModel.Reply("continued")))
    val loop = Loop(model)
    import loop.*
    turn("write it")
    assertEquals(assistants.map(_.text), List("partial", "continued"))
    assert(ui.statuses.contains("resuming"), ui.statuses.toString)
    val secondRequest = model.seenHistories(1)
    assertEquals(secondRequest.last, Msg.Continuation(AgentMessages.truncationContinuation))
    assertEquals(secondRequest.count(_.isInstanceOf[Msg.User]), 1) // no fake user input in transcript accounting

  test("safety and truncation stops never execute accompanying partial tool calls"):
    def call(id: String) = ToolCall(id, Prompts.ToolName, ujson.write(ujson.Obj("code" -> "println(99)")))
    val blocked = completion("", List(call("blocked")), reason = "content_filter", stop = CompletionStop.Blocked)
    val refused = Loop(ScriptedModel("m", Seq(ScriptedModel.Comp(blocked))))
    refused.turn("blocked")
    assertEquals(refused.agent.toolCalls, 0)
    assert(refused.ui.toolStarts.isEmpty)
    assert(refused.ui.warnings.exists(_.contains("ignored 1 tool call")), refused.ui.warnings.toString)
    refused.agent.history.last match
      case Msg.Assistant(text, calls, native) =>
        assert(text.contains("tool calls were not executed"), text)
        assertEquals(calls, Nil)
        assertEquals(native, None)
      case other => fail(s"expected safe assistant marker, got $other")

    val truncated = completion("", List(call("cut")), reason = "max_tokens", stop = CompletionStop.Truncated)
    val cut = Loop(ScriptedModel("m", Seq(ScriptedModel.Comp(truncated), ScriptedModel.Reply("recovered"))))
    cut.turn("cut")
    assertEquals(cut.agent.toolCalls, 0)
    assert(cut.ui.toolStarts.isEmpty)
    assertEquals(cut.agent.history.last, Msg.Assistant("recovered", Nil, None))

  test("a refusal stop reason warns the user"):
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.refusal("I can't help with that."))))
    import loop.*
    turn("do something")
    assert(ui.warnings.exists(_.toLowerCase.contains("refus")))
    assertEquals(agent.history.last, Msg.Assistant("I can't help with that.", Nil, None))

  test("a turn already cancelled does not contact the model"):
    val model = ScriptedModel("m", Seq(ScriptedModel.Reply("unreached")))
    val loop = Loop(model)
    import loop.*
    turn("go", () => true)
    assertEquals(model.i, 0)
    assertEquals(agent.history.last, Msg.Assistant("[interrupted by user]", Nil, None))
    assert(ui.warnings.exists(_.contains("interrupted")), ui.warnings.toString)

  test("cancelling between resume rounds does not create consecutive assistant messages"):
    val model = ScriptedModel("m", Seq(ScriptedModel.unfinished("partial"), ScriptedModel.Reply("next turn")))
    val loop = Loop(model)
    import loop.*
    var cancelled = false
    ui.onEnd = () => cancelled = true
    turn("first", () => cancelled)
    assertEquals(model.i, 1)
    assertEquals(assistants.map(_.text), List("partial"))
    assert(ui.warnings.exists(_.contains("interrupted")), ui.warnings.toString)
    turn("second")
    assertEquals(agent.history.last, Msg.Assistant("next turn", Nil, None))

  test("a CancelledException during streaming ends the turn cleanly and tells the user"):
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Throw(new CancelledException))))
    import loop.*
    turn("go")
    assertEquals(agent.history.last, Msg.Assistant("[interrupted by user]", Nil, None))
    assert(ui.warnings.exists(_.contains("interrupted")), ui.warnings.toString)

  test("manual compaction preserves pending notes, requests, usage and the live REPL"):
    val model = ScriptedModel(
      "m",
      Seq(
        ScriptedModel.Reply("details " * 2000),
        ScriptedModel.Reply("Keep working on the answer; answer is 42."),
        ScriptedModel.Reply("continued"),
      )
    )
    val loop = Loop(model)
    import loop.*
    session.run("val answer = 42")
    turn("remember the answer")
    agent.noteSandboxRestarted("test pending notice")
    val before = agent.snapshot
    assertEquals(agent.compact("keep the answer", () => false), Agent.CompactOutcome.Compacted)
    assertEquals(agent.history.size, 2)
    assertEquals(agent.snapshot.pendingNotes, before.pendingNotes)
    assertEquals(agent.snapshot.userRequests, before.userRequests)
    assertEquals(agent.usageByPurpose.map(_._1), List(Agent.Turns, Agent.Compaction))
    assert(model.seenHistories.last.head.asInstanceOf[Msg.User].text.contains("keep the answer"))
    assert(session.run("answer + 1").success)
    turn("continue")
    assert(agent.history.collect { case Msg.User(t) => t }.last.contains("test pending notice"))

  test("failed, incomplete, empty and larger summaries leave history unchanged"):
    val steps = List(
      ScriptedModel.Throw(RuntimeException("offline")),
      ScriptedModel.unfinished("partial"),
      ScriptedModel.Reply(""),
      ScriptedModel.Reply("expanded " * 4000),
    )
    steps.foreach: step =>
      val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Reply("detail " * 1000), step)))
      import loop.*
      turn("task")
      val before = agent.snapshot
      try assertEquals(agent.compact("", () => false), Agent.CompactOutcome.SummaryNotSmaller)
      catch case _: RuntimeException => ()
      assertEquals(agent.snapshot, before)

  test("cancelled and empty compaction never starts a model request or changes history"):
    val model = ScriptedModel("m", Seq(ScriptedModel.Reply("detail " * 1000)))
    val loop = Loop(model)
    import loop.*
    assertEquals(agent.compact("", () => false), Agent.CompactOutcome.NothingToCompact)
    assertEquals(model.i, 0)
    turn("task")
    val before = agent.snapshot
    intercept[CancelledException](agent.compact("", () => true))
    assertEquals(agent.snapshot, before)
    assertEquals(model.i, 1)

  test("automatic compaction runs before the request that would reach the threshold"):
    val big = "old findings " * 1000
    val (loop, model) = sized(
      Config(autoCompactThreshold = 0.5, compactKeepRatio = 0.02),
      Msg.User("investigate"),
      Msg.Assistant(big, Nil, None)
    ): afterFirst =>
      ScriptedModel(
        "m",
        Seq(
          ScriptedModel.Reply(big),
          ScriptedModel.Reply("Earlier work found the bug in parser.scala."),
          ScriptedModel.Reply("fixed"),
        ),
        contextWindow = Some((afterFirst * 2).toInt)
      )
    import loop.*
    assertEquals(turn("investigate"), TurnOutcome.Finished)
    assertEquals(model.i, 1, "nothing is summarized after the final answer")
    assertEquals(agent.history.size, 2)
    assertEquals(turn("fix it now"), TurnOutcome.Finished)
    assertEquals(model.i, 3)
    val transcript = model.seenHistories(1)
    assertEquals(transcript.size, 1)
    assert(transcript.head.asInstanceOf[Msg.User].text.contains("old findings"))
    val sent = model.seenHistories(2)
    assertEquals(sent.size, 3)
    assertEquals(sent(1), Msg.Assistant("Earlier work found the bug in parser.scala.", Nil, None))
    assertEquals(sent.last, Msg.User("fix it now"))
    assertEquals(agent.history.last, Msg.Assistant("fixed", Nil, None))
    assert(ui.warnings.exists(_.contains("compacted")))
    val saved = agent.snapshot
    agent.restore(saved)
    assertEquals(agent.history, saved.history)

  test("automatic compaction runs when the output reserve leaves less room than the threshold"):
    val big = "old findings " * 1000
    val (loop, model) =
      sized(Config(compactKeepRatio = 0.02), Msg.User("investigate"), Msg.Assistant(big, Nil, None)): afterFirst =>
        // The next request sits at 70% of the window: under the 80% threshold, but over the
        // 60% that a 40% output reserve leaves, so trimming would otherwise cut it.
        val window = (afterFirst / 0.7).toInt
        ScriptedModel(
          "m",
          Seq(
            ScriptedModel.Reply(big),
            ScriptedModel.Reply("Earlier work found the bug."),
            ScriptedModel.Reply("fixed")
          ),
          contextWindow = Some(window),
          maxOutputTokens = Some((window * 0.4).toInt),
        )
    import loop.*
    assertEquals(turn("investigate"), TurnOutcome.Finished)
    assertEquals(turn("fix it now"), TurnOutcome.Finished)
    assertEquals(model.i, 3)
    assert(ui.warnings.exists(_.contains("compacted")), ui.warnings.toString)
    assertEquals(model.seenHistories(2)(1), Msg.Assistant("Earlier work found the bug.", Nil, None))

  test("input queued during a turn can be taken back in order, for the prompt after Ctrl-C"):
    val agent = Loop(ScriptedModel("m", Nil)).agent
    agent.submit("first")
    agent.submit("  ")
    agent.submit("second")
    assertEquals(agent.takeQueuedInput(), List("first", "second"))
    assertEquals(agent.queuedInputCount, 0)

  test("a tool call cut by the output limit is reported as not run and retried a bounded number of times"):
    def cut = ScriptedModel.Comp(completion(
      "Writing the file.",
      List(ToolCall("cut", Prompts.ToolName, """{"code": "write(\"a.txt\", \"""")),
      reason = "max_tokens",
      stop = CompletionStop.Truncated,
    ))
    val model = ScriptedModel("m", Seq(cut, cut, cut, ScriptedModel.Reply("unused")))
    val loop = Loop(model)
    import loop.*
    assertEquals(turn("write a big file"), TurnOutcome.LimitReached)
    assertEquals(model.i, 3)
    assertEquals(agent.toolCalls, 0)
    assertEquals(model.seenHistories(1).last, Msg.Continuation(AgentMessages.truncatedToolCall))
    assert(ui.warnings.exists(_.contains("output limit inside a tool call")), ui.warnings.toString)

  test("automatic compaction failure falls back to whole-exchange trimming and is not retried at once"):
    val big = "old findings " * 1000
    val (loop, model) = sized(Config(), Msg.User("investigate"), Msg.Assistant(big, Nil, None)): afterFirst =>
      // Small enough that the second request reaches 80%, and that trimming has to drop the
      // first exchange; large enough for the transcript to fit the summarizer's allowance.
      ScriptedModel(
        "m",
        Seq(
          ScriptedModel.Reply(big),
          ScriptedModel.Throw(RuntimeException("summary unavailable")),
          ScriptedModel.Reply("continued"),
          ScriptedModel.Reply("again"),
        ),
        contextWindow = Some((afterFirst * 1.05).toInt)
      )
    import loop.*
    assertEquals(turn("investigate"), TurnOutcome.Finished)
    assertEquals(turn("continue"), TurnOutcome.Finished)
    assertEquals(model.seenHistories(2).size, 1)
    assert(
      ui.warnings.exists(_.contains("compaction failed (RuntimeException: summary unavailable)")),
      ui.warnings.toString
    )
    assertEquals(agent.history.last, Msg.Assistant("continued", Nil, None))
    // Still over the threshold, but not enough has been added since the failure to try again.
    assertEquals(turn("more"), TurnOutcome.Finished)
    assertEquals(model.i, 4)
    assertEquals(agent.history.last, Msg.Assistant("again", Nil, None))

  test("a summary that is no smaller is kept out of history and retried only after history grows"):
    val big = "findings " * 2000
    val (loop, model) = sized(
      Config(autoCompactThreshold = 0.5, compactKeepRatio = 0),
      Msg.User("task"),
      Msg.Assistant(big, Nil, None)
    ): afterFirst =>
      ScriptedModel(
        "m",
        Seq(
          ScriptedModel.Reply(big),
          ScriptedModel.Reply("expanded " * 6000),
          ScriptedModel.Reply("second"),
          ScriptedModel.Reply("third"),
          ScriptedModel.Reply("summary"),
          ScriptedModel.Reply("fourth"),
        ),
        contextWindow = Some((afterFirst * 2).toInt)
      )
    import loop.*
    turn("task")
    turn("next")
    assert(ui.warnings.exists(_.contains("no smaller summary")), ui.warnings.toString)
    assertEquals(agent.history.last, Msg.Assistant("second", Nil, None))
    assert(agent.history.forall(_ != Msg.Assistant("expanded " * 6000, Nil, None)))
    turn("again")
    assertEquals(model.i, 4, "no second attempt while history has grown less than a tenth of the window")
    // A big new exchange passes the retry mark and the next request is compacted again.
    turn("x" * model.contextWindow.getOrElse(0))
    assertEquals(model.i, 6)
    assertEquals(agent.history.last, Msg.Assistant("fourth", Nil, None))
    assert(agent.history.contains(Msg.Assistant("summary", Nil, None)))

  test("clear() lifts the retry mark, so a new conversation may compact at its own threshold"):
    val big = "findings " * 2000
    val (loop, model) = sized(
      Config(autoCompactThreshold = 0.5, compactKeepRatio = 0),
      Msg.User("task"),
      Msg.Assistant(big, Nil, None)
    ): afterFirst =>
      ScriptedModel(
        "m",
        Seq(
          ScriptedModel.Reply(big),
          ScriptedModel.Reply("expanded " * 6000), // no smaller: sets the retry mark
          ScriptedModel.Reply("second"),
          ScriptedModel.Reply(big), // the fresh conversation starts here
          ScriptedModel.Reply("summary"),
          ScriptedModel.Reply("third"),
        ),
        contextWindow = Some((afterFirst * 2).toInt)
      )
    import loop.*
    turn("task")
    turn("next")
    assert(ui.warnings.exists(_.contains("no smaller summary")), ui.warnings.toString)

    agent.clear()
    turn("task")
    turn("next")
    assertEquals(model.i, 6, "the second conversation is over its own threshold and must be summarized")
    assert(agent.history.contains(Msg.Assistant("summary", Nil, None)), agent.history.toString.take(200))

  test("automatic compaction uses the configured fraction of the window for the next request"):
    List(0.5 -> true, 0.5001 -> false, 0.0 -> false).foreach: (threshold, expected) =>
      val answer = "findings " * 2000
      val (loop, model) = sized(
        Config(autoCompactThreshold = threshold, compactKeepRatio = 0),
        Msg.User("task"),
        Msg.Assistant(answer, Nil, None),
        Msg.User("again")
      ): nextRequest =>
        ScriptedModel(
          "m",
          Seq(ScriptedModel.Reply(answer), ScriptedModel.Reply("summary"), ScriptedModel.Reply("second")),
          contextWindow = Some((nextRequest * 2).toInt)
        )
      assertEquals(loop.turn("task"), TurnOutcome.Finished)
      assertEquals(model.i, 1)
      assertEquals(loop.turn("again"), TurnOutcome.Finished)
      assertEquals(model.i, if expected then 3 else 2)
      val second = model.seenHistories(1).head.asInstanceOf[Msg.User].text
      assertEquals(second.startsWith("Summary focus"), expected, second)

  test("compaction between tool rounds summarizes the exchange in progress and asks the model to continue"):
    val config = Config(autoCompactThreshold = 0.5, compactKeepRatio = 0)
    val (loop, model) = sized(config, Msg.User("task")): firstRequest =>
      ScriptedModel(
        "m",
        Seq(
          ScriptedModel.tool("println(\"finding \" * 3000)"),
          ScriptedModel.Reply("summary"),
          ScriptedModel.Reply("done")
        ),
        contextWindow = Some((firstRequest * 2 + 2).toInt) // the tool exchange tips the second round over
      )
    import loop.*
    assertEquals(turn("task"), TurnOutcome.Finished)
    assertEquals(model.i, 3)
    assertEquals(model.seenHistories(0), List(Msg.User("task")))
    val transcript = model.seenHistories(1).head.asInstanceOf[Msg.User].text
    assert(transcript.contains("println") && transcript.contains("finding finding"), transcript.take(400))
    val resumed = model.seenHistories(2)
    assertEquals(resumed.size, 3)
    assert(resumed.head.asInstanceOf[Msg.User].text.startsWith("[compacted conversation]"), resumed.head.toString)
    assertEquals(resumed(1), Msg.Assistant("summary", Nil, None))
    assertEquals(resumed(2), Msg.Continuation(AgentMessages.compactionContinuation))
    assertEquals(agent.history.last, Msg.Assistant("done", Nil, None))

  test("cancelling an automatic compaction interrupts the turn and leaves history unchanged"):
    val answer = "findings " * 2000
    val (loop, model) = sized(
      Config(autoCompactThreshold = 0.5, compactKeepRatio = 0),
      Msg.User("task"),
      Msg.Assistant(answer, Nil, None),
      Msg.User("next")
    ): nextRequest =>
      ScriptedModel(
        "m",
        Seq(ScriptedModel.Reply(answer), ScriptedModel.Throw(CancelledException())),
        contextWindow = Some((nextRequest * 2).toInt)
      )
    import loop.*
    // Ctrl-C arrives while the summary request (the second call) is in flight.
    val cancelled = () => model.i >= 2
    assertEquals(turn("task", cancelled), TurnOutcome.Finished)
    assertEquals(turn("next", cancelled), TurnOutcome.Interrupted)
    assertEquals(model.i, 2)
    assertEquals(agent.history.take(3), List(Msg.User("task"), Msg.Assistant(answer, Nil, None), Msg.User("next")))
    assertEquals(agent.history.drop(3), List(Msg.Assistant(AgentMessages.interrupted, Nil, None)))
    assert(ui.warnings.exists(_.contains("interrupted")), ui.warnings.toString)

  test("a model with a context window sees the oldest exchanges dropped, and is told"):
    val big = "y" * 8000 // ~2000 tokens
    val model = ScriptedModel(
      "m",
      Seq(ScriptedModel.Reply(big), ScriptedModel.Reply(big), ScriptedModel.Reply("third")),
      contextWindow = Some(6000)
    )
    val loop = Loop(model, Config(autoCompactThreshold = 0))
    import loop.*
    turn("first question")
    turn("second question")
    // by now: user, big, user, big (~4000 tokens of history) + system prompt; the third turn must cut
    turn("third question")
    val seen = model.seenHistories.last
    assert(seen.size < 5, seen.map(_.toString.take(40)).toString)
    assert(seen.head.isInstanceOf[Msg.User])
    assert(seen.head.asInstanceOf[Msg.User].text.startsWith("[context notice]"), seen.head.toString.take(120))
    assert(ui.warnings.exists(_.contains("context window")), ui.warnings.toString)
    // the history itself is cut (what was shown stays in the terminal), and starts with the notice
    assertEquals(agent.history.count(_.isInstanceOf[Msg.User]), 1)
    assert(agent.history.head.asInstanceOf[Msg.User].text.contains("oldest messages"), agent.history.head.toString)

  test("context fitting warns when required input cannot fit, including the configured output reserve"):
    val tiny = ScriptedModel("tiny", Seq(ScriptedModel.Reply("done")), contextWindow = Some(1000))
    val small = Loop(tiny, Config(autoCompactThreshold = 0))
    small.turn("short request")
    assert(small.ui.warnings.exists(_.contains("system prompt and tool schema alone")), small.ui.warnings.toString)
    assertEquals(tiny.i, 1) // the advisory warning does not make local/custom models unusable

    val huge = ScriptedModel(
      "reserved",
      Seq(ScriptedModel.Reply("done")),
      contextWindow = Some(100_000),
      maxOutputTokens = Some(60_000)
    )
    val reserved = Loop(huge)
    reserved.turn("x" * 200_000)
    val warnings = reserved.ui.warnings
    val warning = warnings.find(_.contains("latest retained exchange")).getOrElse(warnings.mkString("; "))
    assert(warning.contains("configured maxTokens=60000"), warning)
    assert(warning.contains("provider may reject"), warning)

  test("contextUsage estimates the next request, grows with the history, and follows the provider's count"):
    // TokenUsage(1, 1) per completion is below CalibrationMinTokens, so the estimate stays chars/4.
    val plain = Loop(ScriptedModel("m", Seq(ScriptedModel.Reply("a" * 4000), ScriptedModel.Reply("b")), Some(100_000)))
    val before = plain.agent.contextUsage
    assertEquals(before.window, Some(100_000))
    assert(before.tokens > 0, before.toString) // the system prompt and tool schema alone count
    plain.turn("hello")
    val after = plain.agent.contextUsage
    assert(after.tokens >= before.tokens + 1000, s"$before -> $after") // ~1000 tokens of reply appended
    // A real provider count calibrates the estimate: a completion reporting a
    // prompt twice the estimate doubles what is shown.
    val big = ScriptedModel.Comp(completion("c", usage = TokenUsage(after.tokens * 2, 1)))
    val calibrated = Loop(ScriptedModel("m", Seq(big), Some(100_000)))
    import calibrated.agent
    calibrated.turn("hello")
    val estimate = agent.contextUsage.tokens
    val raw = ContextManager.estimateTokens(agent.systemPrompt) + agent.history.map(ContextManager.estimateTokens).sum
    assert(estimate > raw * 3 / 2, s"estimate $estimate raw $raw")
    agent.clear()
    assertEquals(agent.contextUsage.window, Some(100_000))
    assert(agent.contextUsage.tokens < after.tokens, "clear() leaves only the fixed part")

  test("switching the agent model resets token calibration learned from the old tokenizer"):
    val inflated = ScriptedModel.Comp(completion("ok", usage = TokenUsage(100_000, 1)))
    val loop = Loop(ScriptedModel("old", Seq(inflated), Some(1_000_000)))
    import loop.*
    turn("calibrate")
    val calibrated = agent.contextUsage.tokens
    agent.model = ScriptedModel("new", Nil, Some(1_000_000))
    val reset = agent.contextUsage.tokens
    assert(reset < calibrated / 2, s"calibrated=$calibrated reset=$reset")

  test("context estimates omit native reasoning that the newly selected model cannot replay"):
    val native = NativeTurn("scripted", "old", "r" * 40_000)
    val loop = Loop(ScriptedModel("old", Seq(ScriptedModel.Comp(completion("short", native = Some(native))))))
    import loop.*
    turn("go")
    val onOrigin = agent.contextUsage.tokens
    agent.model = ScriptedModel("new", Nil)
    val onOther = agent.contextUsage.tokens
    assert(onOrigin - onOther >= 9000, s"origin=$onOrigin other=$onOther")

  test("what the user decided at a prompt is reported in that tool result; the system prompt never changes"):
    // The model cannot see the pop-ups, so each decision is said in the result
    // (once vs. session vs. denied); the system prompt stays the same for the whole
    // session so every request keeps its cacheable prefix.
    val command = ProcessFixture.command("pwd")
    val pattern = ProcessFixture.pattern("pwd")
    val code =
      s"""requestExec(Set(${ujson.write(pattern)}), "list the directory") { exec(${ujson.write(command)}).exitCode }"""
    val loop = Loop(
      ScriptedModel(
        "m",
        Seq(ScriptedModel.tool(code), ScriptedModel.tool(code), ScriptedModel.tool(code), ScriptedModel.Reply("done"))
      ),
      commands = Nil,
    )
    import loop.*
    env.decisions = List(Decision.AllowOnce, Decision.AllowSession)
    val promptBefore = agent.systemPrompt
    assert(promptBefore.contains("Current permissions"), promptBefore)
    turn("list it")
    val results = toolResults.flatMap(_.results)
    assertEquals(results.size, 3)
    assert(
      results(0).output.contains(s"[permissions: the user allowed commands $pattern once (this call only"),
      results(0).output
    )
    assert(
      results(1).output.contains(s"[permissions: the user allowed commands $pattern for the rest of this session"),
      results(1).output
    )
    assert(!results(2).output.contains("[permissions:"), results(2).output) // covered by the session grant: no prompt
    assertEquals(env.requests.size, 2) // once, then session, then nothing to ask
    assertEquals(agent.systemPrompt, promptBefore)
    assert(!agent.systemPrompt.contains(pattern))

  test("permission feedback stops the old batch and reaches the model before a narrower request"):
    val commands = List("one", "two", "three", "four", "five")
    def request(patterns: List[String], target: String): String =
      s"""requestExec(Set(${patterns.map(ujson.write(_)).mkString(", ")}), "run tasks") {
         |  write(${ujson.write(target)}, "executed")
         |}""".stripMargin
    val original = s"try ${request(commands, "original.txt")} catch case _: SecurityException => ()"
    val stale = """write("stale.txt", "executed")"""
    val revised = request(commands.init, "revised.txt")
    val model = ScriptedModel(
      "m",
      Seq(ScriptedModel.tools(original, stale), ScriptedModel.tool(revised), ScriptedModel.Reply("done"))
    )
    val loop = Loop(model, Config(maxToolOutputChars = 20), commands = Nil)
    import loop.*
    val instructions = "Request only one, two, three and four; skip five."
    env.decisions = List(Decision.Revise(instructions), Decision.AllowOnce)

    turn("run the tasks")

    assert(!Files.exists(env.root.resolve("original.txt")))
    assert(!Files.exists(env.root.resolve("stale.txt")))
    assertEquals(env.contents("revised.txt"), "executed")
    val first = toolResults.head.results
    assert(!first.head.isError, first.head.output) // the snippet caught the permission exception
    assert(first.head.needsReplan)
    assert(first.head.output.contains(instructions), first.head.output)
    assertEquals(first(1).output, AgentMessages.skippedAfterFeedback)
    assertEquals(model.seenHistories(1).last, Msg.ToolResults(first))
    assertEquals(env.requests.size, 2)
    assertEquals(
      env.requests.collect { case r: ExecRequest => r.commands.toSet }.toList,
      List(commands.toSet, commands.init.toSet)
    )
    assertEquals(env.policy.openScopeCount, 0)
    assertEquals(env.policy.sessionGrants, Nil)
    assertEquals(agent.toolCalls, 2)

  test("usage is accumulated across the turn and reset by clear()"):
    val loop = Loop(ScriptedModel(
      "m",
      Seq(
        ScriptedModel.tool("1 + 1"),
        ScriptedModel.Reply("done"),
      )
    ))
    import loop.*
    turn("go")
    assertEquals(agent.usage.input, 2L) // two completions, TokenUsage(1,1) each
    assertEquals(agent.usage.output, 2L)
    assert(agent.toolCalls >= 1)
    // other model calls (chat(), predictions) are recorded under their own purpose and add to the total
    agent.recordUsage(Agent.Prediction, TokenUsage(10, 5))
    agent.recordUsage(Agent.Prediction, TokenUsage(10, 5))
    assertEquals(agent.usage, TokenUsage(22, 12))
    assertEquals(agent.usageByPurpose, List(Agent.Turns -> TokenUsage(2, 2), Agent.Prediction -> TokenUsage(20, 10)))
    agent.clear()
    assertEquals(agent.history, Nil)
    assertEquals(agent.usage, TokenUsage())
    assertEquals(agent.usageByPurpose, Nil)
    assertEquals(agent.toolCalls, 0)

  test("history threads across turns and state persists in the session"):
    val loop = Loop(ScriptedModel(
      "m",
      Seq(
        ScriptedModel.tool("val remembered = 7"),
        ScriptedModel.Reply("stored"),
        ScriptedModel.tool("remembered * 6"),
        ScriptedModel.Reply("42"),
      )
    ))
    import loop.*
    turn("store")
    turn("use it")
    assertEquals(agent.history.count { case _: Msg.User => true; case _ => false }, 2)
    assert(toolResults.last.results.head.output.contains("42"))

  test("the native replay payload is carried on the assistant message"):
    val native = NativeTurn("scripted", "scripted-model", "opaque-payload")
    val loop = Loop(ScriptedModel("m", Seq(ScriptedModel.Comp(completion("with native", native = Some(native))))))
    import loop.*
    turn("go")
    assertEquals(agent.history.last, Msg.Assistant("with native", Nil, Some(native)))
    assert(native.isFor("scripted", "scripted-model"))
    assert(!native.isFor("scripted", "another-model"))

  test("manual and automatic compaction preserve recent exchanges within the configured window fraction"):
    List(false, true).foreach: automatic =>
      val older = List(Msg.User("old task"), Msg.Assistant("old findings " * 4000, Nil, None))
      val recent = List(Msg.User("recent task"), Msg.Assistant("recent findings", Nil, None))
      val steps = if automatic then Seq(ScriptedModel.Reply("old summary"), ScriptedModel.Reply("latest answer"))
      else Seq(ScriptedModel.Reply("old summary"))
      val model = ScriptedModel("m", steps, contextWindow = Some(100000))
      val loop = Loop(model, Config(autoCompactThreshold = 0.1, compactKeepRatio = 0.01))
      import loop.*
      agent.restore(agent.snapshot.copy(history = older ++ recent))
      if automatic then assertEquals(turn("latest task"), TurnOutcome.Finished)
      else assertEquals(agent.compact("", () => false), Agent.CompactOutcome.Compacted)
      assertEquals(agent.history.slice(2, 4), recent)
      assertEquals(agent.history(1), Msg.Assistant("old summary", Nil, None))
      if automatic then assertEquals(agent.history.last, Msg.Assistant("latest answer", Nil, None))
      val transcript = model.seenHistories.head.head.asInstanceOf[Msg.User].text
      assert(transcript.contains("old findings"))
      assert(!transcript.contains("recent findings"))
      val before = agent.snapshot
      assertEquals(agent.compact("", () => false), Agent.CompactOutcome.NothingToCompact)
      assertEquals(agent.snapshot, before)
      assertEquals(model.i, steps.size)
