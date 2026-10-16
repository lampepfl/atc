package atc

import atc.agent.{AgentUI, Prompts}
import atc.llm.*
import atc.sandbox.ExecutionResult

import scala.collection.mutable.ListBuffer

/** A completion with the fields a test leaves out filled in. */
def completion(
  text: String = "answer",
  calls: List[ToolCall] = Nil,
  native: Option[NativeTurn] = None,
  reason: String = "end_turn",
  stop: CompletionStop = CompletionStop.Complete,
  usage: TokenUsage = TokenUsage(1, 1),
): Completion = Completion(text, calls, native, usage, reason, stop)

/** A programmable model: returns a scripted sequence of completions, records
  * the history it is asked to complete, and can throw on demand. */
final class ScriptedModel(
  val alias: String,
  steps: Seq[ScriptedModel.Step],
  override val contextWindow: Option[Int] = None,
  override val maxOutputTokens: Option[Int] = None
) extends ChatModel:
  val modelId = "scripted"; val providerKey = "scripted"; val webSearch = false
  val seenHistories: ListBuffer[List[Msg]] = ListBuffer()
  var i = 0
  def complete(
    system: SystemPrompt,
    history: List[Msg],
    tools: List[ToolSpec],
    sink: StreamSink,
    cancelled: () => Boolean
  ): Completion =
    seenHistories += history
    val step = if i < steps.length then steps(i) else ScriptedModel.Reply("(no more steps)")
    i += 1
    step match
      case ScriptedModel.Throw(e) => throw e
      case ScriptedModel.Comp(c) =>
        sink.text(c.text)
        c
      case ScriptedModel.Reply(t) =>
        sink.text(t)
        completion(t)
  def simple(system: Option[String], prompt: String, thinking: Boolean): Reply =
    Reply(s"scripted: $prompt", TokenUsage(1, 1))

object ScriptedModel:
  sealed trait Step
  case class Comp(c: Completion) extends Step
  case class Reply(text: String) extends Step
  case class Throw(e: Throwable) extends Step
  private var seq = 0
  private def nextId(): String = { seq += 1; s"call-$seq" }
  private def call(code: String): ToolCall =
    ToolCall(nextId(), Prompts.ToolName, ujson.write(ujson.Obj("code" -> code)))
  /** A completion that calls `run_scala` with `code`. */
  def tool(code: String, text: String = "running"): Comp = Comp(completion(text, List(call(code)), reason = "tool_use"))
  /** A completion with several tool calls at once. */
  def tools(codes: String*): Comp = Comp(completion("multi", codes.toList.map(call), reason = "tool_use"))
  def unfinished(text: String): Comp = Comp(completion(text, reason = "pause_turn", stop = CompletionStop.Resume))
  def refusal(text: String): Comp = Comp(completion(text, reason = "refusal", stop = CompletionStop.Blocked))

/** A recording AgentUI. */
final class RecordingUI extends AgentUI:
  /** What the double answers when the tool budget is used up, and how often it was asked. */
  var allowMoreToolCalls: Boolean = false
  var budgetAsks: Int = 0
  override def confirmMoreToolCalls(used: Int, budget: Int): Boolean =
    budgetAsks += 1
    allowMoreToolCalls
  val deltas, notes, statuses, warnings, toolStarts = ListBuffer[String]()
  val toolEnds = ListBuffer[Boolean]()
  var ends = 0
  var onEnd: () => Unit = () => ()
  /** Called with each streamed piece of answer text, on the model request's thread. */
  var onDelta: String => Unit = _ => ()
  def assistantDelta(text: String): Unit =
    deltas += text
    onDelta(text)
  def assistantNote(text: String): Unit = notes += text
  def assistantEnd(): Unit =
    ends += 1
    onEnd()
  def toolStart(code: String): Unit = toolStarts += code
  def toolEnd(result: ExecutionResult, millis: Long): Unit = toolEnds += result.success
  def status(text: String): Unit = statuses += text
  def warn(text: String): Unit = warnings += text
  def thinkingDelta(text: String): Unit = ()
