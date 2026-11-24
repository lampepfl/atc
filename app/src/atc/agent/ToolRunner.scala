package atc.agent

import atc.llm.{SdkJson, ToolCall, ToolResult, ToolSpec}
import atc.perms.{Decision, Policy}
import atc.sandbox.{ExecutionResult, ReplSession}

/** Executes the tools exposed to the model. The agent loop owns when a tool may
  * run; a runner owns the tool-specific decoding and effects. */
private[atc] trait ToolRunner:
  def tools: List[ToolSpec]
  def run(call: ToolCall): ToolResult

/** The model's Scala REPL tool, bound to the sandbox session for one turn. */
private[atc] final class ScalaToolRunner(
  session: => ReplSession,
  policy: Policy,
  ui: AgentUI,
  maxOutputChars: Int,
  cancelled: () => Boolean,
) extends ToolRunner:
  val tools: List[ToolSpec] = ScalaToolRunner.tools
  private lazy val repl = session

  def run(call: ToolCall): ToolResult = call.name match
    case Prompts.ToolName => runScala(call)
    case other =>
      ToolResult(
        call.id,
        AgentMessages.unknownTool(other, Prompts.ToolName),
        isError = true
      )

  private def runScala(call: ToolCall): ToolResult =
    val arguments = SdkJson.parseObject(call.arguments)
    val code = arguments.value.get("code").flatMap(_.strOpt).getOrElse("")
    if code.trim.isEmpty then ToolResult(call.id, AgentMessages.missingCodeArgument, isError = true)
    else
      ScalaToolRunner.texts(arguments) match
        case Left(problem) => ToolResult(call.id, AgentMessages.badTextsArgument(problem), isError = true)
        case Right(texts) =>
          ui.toolStart(code, texts)
          val current = repl // may start the sandbox first: not part of the snippet's time
          val (result, decisions) = ScalaToolRunner.evaluate(current, policy, ui, code, texts.toMap, cancelled)
          val rendered = ToolOutput.renderForModel(result, maxOutputChars, decisions, code)
          val needsReplan = decisions.exists {
            case (Decision.Revise(_), _) => true
            case _ => false
          }
          ToolResult(call.id, rendered, isError = !result.success, needsReplan = needsReplan)

private[atc] object ScalaToolRunner:
  /** The native Scala tool; other operations are library calls. */
  val tools: List[ToolSpec] =
    List(ToolSpec(Prompts.ToolName, Prompts.toolDescription, Prompts.toolParameters))

  /** The `texts` of a call's `arguments`, names to strings in the order sent; none when the
    * argument is absent. Left: what the argument was instead of an object of strings. */
  def texts(arguments: ujson.Obj): Either[String, List[(String, String)]] =
    arguments.value.get("texts") match
      case None | Some(ujson.Null) => Right(Nil)
      case Some(ujson.Obj(fields)) =>
        fields.toList.collectFirst { case (name, value) if value.strOpt.isEmpty => name } match
          case Some(name) => Left(s"the value of \"$name\" is not a string")
          case None => Right(fields.toList.map((name, value) => name -> value.str))
      case Some(_: ujson.Str) => Left("it is a string; send the object itself, not its JSON text")
      case Some(_: ujson.Arr) => Left("it is an array")
      case Some(_) => Left("it is a number or a boolean")

  /** Run `code` inside an open tool block (`ui.toolStart` already called): report the result
    * with the evaluation time, minus the time spent waiting for the user at prompts, and
    * return it with what the user decided at those prompts. Shared with the user's `/run`.
    * A stop that `cancelled` shows keeps the code from running. */
  def evaluate(
    session: ReplSession,
    policy: Policy,
    ui: AgentUI,
    code: String,
    texts: Map[String, String],
    cancelled: () => Boolean,
  ): (ExecutionResult, List[(Decision, String)]) =
    val decisionsBefore = policy.decisionCount
    ui.status(AgentMessages.runningScalaStatus)
    val start = System.nanoTime()
    val result = session.run(code, texts, cancelled)
    val millis = (System.nanoTime() - start - session.clock.paused) / 1_000_000L
    ui.toolEnd(result, millis)
    (result, policy.decisionsSince(decisionsBefore))
