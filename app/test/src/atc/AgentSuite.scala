package atc

import atc.agent.*
import atc.config.Config
import atc.lib.TodoStatus
import atc.llm.*
import atc.perms.*
import atc.sandbox.ReplSession

/** End to end: echo model → agent loop → sandbox REPL → real host → policy. */
class AgentSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  private val echoCommand = ProcessFixture.command("echo")

  private val env = TestEnv(TestEnv.withSecrets, commands = List(ProcessFixture.pattern("echo")), prefix = "atc-agent")
  env.file("hello.txt", "hello world")
  env.file("secrets/token.txt", "TOKEN-123")
  private lazy val session: ReplSession = env.newSession()
  private val ui = RecordingUI()
  private val agent =
    Agent(
      Config(),
      AgentEnvironment.current(env.root),
      env.policy,
      ui,
      EchoModel("echo"),
      Some(EchoModel("safe")),
      None
    )

  override def beforeAll(): Unit = session

  override def beforeEach(context: BeforeEach): Unit =
    env.clearOutput()
    env.requests.clear()

  /** Whether the last tool call compiled and ran. */
  private def lastSucceeded: Boolean = ui.toolEnds.last
  private def lastToolText: String = agent.history.collect { case Msg.ToolResults(rs) =>
    rs.map(_.output).mkString
  }.last

  test("plain chat"):
    agent.turn(session, "hi", () => false)
    assertEquals(agent.history.last, Msg.Assistant("echo: hi", Nil, None))

  test("tool call: read a file and print"):
    agent.turn(session, """run: println(read("hello.txt").toUpperCase)""", () => false)
    assert(lastSucceeded, lastToolText)
    assert(env.agentOut.toString.contains("HELLO WORLD"))
    assert(lastToolText.contains("HELLO WORLD"), lastToolText)

  test("compile error is reported to the model, not executed"):
    agent.turn(session, """run: val leaked = requestFiles("/tmp") { access("/tmp") }""", () => false)
    assert(!lastSucceeded)
    assert(lastToolText.toLowerCase.contains("leak"), lastToolText)

  test("classified: model sees mask, user sees content, classified model used"):
    agent.turn(
      session,
      """run: val c = readClassified("secrets/token.txt"); println(c); println(classifiedChat(c.map(_ + "?")))""",
      () => false
    )
    assert(lastSucceeded, lastToolText)
    assert(env.agentOut.toString.contains("Classified(***)"))
    assert(!env.agentOut.toString.contains("TOKEN-123"), env.agentOut.toString)
    assert(!lastToolText.contains("TOKEN-123"), lastToolText)
    assert(env.userOut.toString.contains("<TOKEN-123"), env.userOut.toString)
    assert(env.userOut.toString.contains("safe:TOKEN-123?"), env.userOut.toString)

  test("denied plain read of classified path explains the alternative"):
    agent.turn(session, """run: read("secrets/token.txt")""", () => false)
    assert(!lastSucceeded)
    assert(lastToolText.contains("readClassified"), lastToolText)

  test("permission request flow: denied then granted"):
    val outside = TestEnv.outsideDir("outside!")
    env.decisions = List(Decision.Deny)
    val outsideCode = env.scalaString(outside)
    val fileCode = env.scalaString(outside.resolve("o.txt"))
    agent.turn(
      session,
      s"""run: requestFiles($outsideCode, Access.Read, "need it") { read($fileCode) }""",
      () => false
    )
    assert(!lastSucceeded)
    assert(lastToolText.contains("denied"), lastToolText)
    env.decisions = List(Decision.AllowOnce)
    agent.turn(
      session,
      s"""run: requestFiles($outsideCode, Access.Read, "need it") { read($fileCode) }""",
      () => false
    )
    assert(lastSucceeded, lastToolText)
    assert(lastToolText.contains("outside!"), lastToolText)
    assertEquals(env.requests.size, 2)
    assertEquals(env.requests.last.asInstanceOf[FileRequest].reason, "need it")
    // scope closed after the block: no lingering access
    agent.turn(session, s"run: read($fileCode)", () => false)
    assert(!lastSucceeded)

  test("exec through policy"):
    agent.turn(
      session,
      s"run: println(exec(${ujson.write(echoCommand)}, List(\"from-exec\")).stdout.trim)",
      () => false,
    )
    assert(lastSucceeded, lastToolText)
    assert(lastToolText.contains("from-exec"))
    agent.turn(session, """run: exec("ls")""", () => false)
    assert(!lastSucceeded)
    assert(lastToolText.contains("requestExec"), lastToolText)

  test("state persists across tool calls"):
    agent.turn(session, "run: def twice(s: String) = s + s", () => false)
    agent.turn(session, """run: println(twice("ab"))""", () => false)
    assert(lastToolText.contains("abab"), lastToolText)

  test("ask() reaches the user and returns the answer"):
    env.answers = List(Some("Blue"))
    agent.turn(session, """run: println(ask("Which colour?", List("Red", "Blue")))""", () => false)
    assertEquals(env.questions.last, ("Which colour?", List("Red", "Blue"), false))
    assert(lastToolText.contains("Some(Blue)"), lastToolText)
    agent.turn(session, """run: println(ask("Anything else?"))""", () => false)
    assert(lastToolText.contains("None"), lastToolText)

  test("ask() is an effect: unusable inside Classified.map"):
    agent.turn(session, """run: classify("x").map(s => ask(s))""", () => false)
    assert(!lastSucceeded)

  test("setTodos/markTodo update the list shown to the user"):
    agent.turn(
      session,
      """run: setTodos(List(Todo("write build file", TodoStatus.Done), Todo("add tests", TodoStatus.InProgress), Todo("run tests")))""",
      () => false
    )
    assert(lastSucceeded, lastToolText)
    assertEquals(
      env.shownTodos.map(t => (t.text, t.status.toString)),
      List(("write build file", "Done"), ("add tests", "InProgress"), ("run tests", "Pending"))
    )
    agent.turn(
      session,
      """run: markTodo("add tests", TodoStatus.Done); println(todos.count(_.status == TodoStatus.Done))""",
      () => false
    )
    assert(lastToolText.contains("2"), lastToolText)
    assertEquals(env.host.currentTodos.count(_.status == TodoStatus.Done), 2)

  test("cancellation between tool calls keeps history consistent"):
    var cancelled = false
    agent.turn(session, "run: 1 + 1", () => { val c = cancelled; cancelled = true; c })
    // history must end with an assistant message (no dangling tool results)
    assert(agent.history.last.isInstanceOf[Msg.Assistant])
