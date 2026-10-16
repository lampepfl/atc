package atc.agent

import atc.host.HostInteraction
import atc.lib.{TaskNotes, Todo, TodoStatus}
import atc.llm.{Msg, ToolCall, ToolResult}
import atc.platform.OwnerOnlyFiles

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.HexFormat
import scala.util.Using

/** Portable conversation state. Runtime capabilities, permissions and native provider payloads are excluded. */
private[atc] final case class SessionSnapshot(
  history: List[Msg],
  pendingNotes: List[String],
  userRequests: List[String],
  task: TaskNotes,
  todos: List[Todo],
  model: String,
):
  def nonEmpty: Boolean =
    history.nonEmpty || pendingNotes.nonEmpty || userRequests.nonEmpty || task != TaskNotes() || todos.nonEmpty

private[atc] object SessionStore:
  private val MaxBytes = 8 * 1024 * 1024

  /** Keep automatic saves outside the project and separate them by canonical working directory. */
  def autoSavePath(home: Path, cwd: Path): Path =
    val digest = MessageDigest.getInstance("SHA-256").nn.digest(cwd.toRealPath().nn.toString.getBytes(UTF_8))
    home.resolve(".atc/sessions").nn.resolve(s"${HexFormat.of().nn.formatHex(digest)}.json").nn

  /** Publish a complete checkpoint. A failed write leaves the previous save available. */
  def checkpoint(path: Path, session: SessionSnapshot): Unit =
    OwnerOnlyFiles.replace(path.toAbsolutePath.nn.normalize.nn, encodeWithinLimit(session), keepPermissions = false)

  /** Save to a new file; throws `FileAlreadyExistsException` when `path` exists. */
  def write(path: Path, session: SessionSnapshot): Unit =
    val bytes = encodeWithinLimit(session)
    val target = path.toAbsolutePath.nn.normalize.nn
    Files.createDirectories(target.getParent)
    OwnerOnlyFiles.create(target, bytes)

  private def encodeWithinLimit(session: SessionSnapshot): Array[Byte] =
    val bytes = encode(session).getBytes(UTF_8)
    if bytes.length > MaxBytes then throw IllegalArgumentException("Saved session exceeds the 8 MiB limit")
    bytes

  def read(path: Path): SessionSnapshot =
    val bytes = Using.resource(Files.newInputStream(path).nn)(_.readNBytes(MaxBytes + 1).nn)
    if bytes.length > MaxBytes then throw IllegalArgumentException("Saved session exceeds the 8 MiB limit")
    decode(String(bytes, UTF_8))

  /** A neutral message as saved sessions and the compaction transcript write it, without
    * native replay payloads or the internal `needsReplan` flag. */
  private[agent] def messageJson(message: Msg): ujson.Obj = message match
    case Msg.User(text) => ujson.Obj("role" -> "user", "text" -> text)
    case Msg.Continuation(text) => ujson.Obj("role" -> "continuation", "text" -> text)
    case Msg.Assistant(text, calls, _) =>
      val entries = calls.map(call => ujson.Obj("id" -> call.id, "name" -> call.name, "arguments" -> call.arguments))
      ujson.Obj("role" -> "assistant", "text" -> text, "calls" -> ujson.Arr.from(entries))
    case Msg.ToolResults(results) =>
      val entries =
        results.map(result => ujson.Obj("id" -> result.callId, "output" -> result.output, "error" -> result.isError))
      ujson.Obj("role" -> "tools", "results" -> ujson.Arr.from(entries))

  private[atc] def encode(session: SessionSnapshot): String =
    def strings(values: List[String]): ujson.Arr = ujson.Arr.from(values)
    ujson.write(
      ujson.Obj(
        "version" -> 1,
        "model" -> session.model,
        "history" -> ujson.Arr.from(session.history.map(messageJson)),
        "pendingNotes" -> strings(session.pendingNotes),
        "userRequests" -> strings(session.userRequests),
        "task" -> ujson.Obj(
          "goal" -> session.task.goal,
          "constraints" -> strings(session.task.constraints),
          "completed" -> strings(session.task.completed),
          "remaining" -> strings(session.task.remaining)
        ),
        "todos" ->
          ujson.Arr.from(session.todos.map(todo => ujson.Obj("text" -> todo.text, "status" -> todo.status.toString))),
      ),
      indent = 2
    )

  private[atc] def decode(text: String): SessionSnapshot =
    val data = ujson.read(text)
    if data("version").num != 1 then throw IllegalArgumentException("Unsupported saved-session version")
    def strings(value: ujson.Value): List[String] = value.arr.toList.map(_.str)
    val history = data("history").arr.toList.map: value =>
      value("role").str match
        case "user" => Msg.User(value("text").str)
        case "continuation" => Msg.Continuation(value("text").str)
        case "assistant" =>
          val calls = value("calls").arr.toList.map(c => ToolCall(c("id").str, c("name").str, c("arguments").str))
          Msg.Assistant(value("text").str, calls, None)
        case "tools" =>
          val results = value("results").arr.toList.map(r => ToolResult(r("id").str, r("output").str, r("error").bool))
          Msg.ToolResults(results)
        case other => throw IllegalArgumentException(s"Unknown saved message role: $other")
    var pending = Set.empty[String]
    history.foreach:
      case Msg.ToolResults(results) =>
        if results.map(_.callId).toSet != pending || results.size != pending.size || pending.isEmpty then
          throw IllegalArgumentException("Saved session has mismatched tool results")
        pending = Set.empty
      case message =>
        if pending.nonEmpty then throw IllegalArgumentException("Saved session has unanswered tool calls")
        message match
          case Msg.Assistant(_, calls, _) =>
            pending = calls.map(_.id).toSet
            if pending.size != calls.size then
              throw IllegalArgumentException("Saved session has duplicate tool call IDs")
          case _ => ()
    if pending.nonEmpty then throw IllegalArgumentException("Saved session has unanswered tool calls")
    val task = data("task")
    val notes =
      TaskNotes(task("goal").str, strings(task("constraints")), strings(task("completed")), strings(task("remaining")))
    if HostInteraction.length(notes) > HostInteraction.MaxTaskNotesChars then
      throw IllegalArgumentException(s"Saved task notes exceed ${HostInteraction.MaxTaskNotesChars} characters")
    val requests = strings(data("userRequests"))
    if requests.size > Conversation.RecentRequests + 1 || requests.exists(_.length > Conversation.MaxRequestChars) then
      throw IllegalArgumentException("Saved user request history exceeds its limit")
    SessionSnapshot(
      history,
      strings(data("pendingNotes")),
      requests,
      notes,
      data("todos").arr.toList.map(t => Todo(t("text").str, TodoStatus.valueOf(t("status").str))),
      data("model").str
    )
