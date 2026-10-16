package atc.host

import atc.lib.*

import scala.util.{Failure, Success, Try}

/** User-visible output, questions, TODO state, and model interaction supplied
  * by [[Host]]. */
private[host] trait HostInteraction:
  self: Host =>

  @volatile private var todoList: List[Todo] = Nil
  @volatile private var notes = TaskNotes()
  /** Serializes the TODO updates, which `parallel` tasks can make at once. */
  private val todoLock = Object()

  def setTaskNotes(value: TaskNotes)(using UserIO): Unit =
    if HostInteraction.length(value) > HostInteraction.MaxTaskNotesChars then
      throw IllegalArgumentException(
        s"Task notes exceed ${HostInteraction.MaxTaskNotesChars} characters; summarize completed work."
      )
    notes = value

  def taskNotes(using UserIO): TaskNotes = notes
  private[atc] def currentTaskNotes: TaskNotes = notes
  private[atc] def restoreTaskState(value: TaskNotes, todos: List[Todo]): Unit = todoLock.synchronized:
    notes = value
    todoList = todos

  /** Report a classified computation failure only through the user channel;
    * exposing the failure bit to the agent could reveal classified data. */
  private[host] def classifiedSinkFailed(operation: String): Unit =
    output.print(
      "",
      s"<$operation failed: the classified value is the result of a failed computation; its error is confidential>\n"
    )

  private def agentView(value: Any): Any = value match
    case _: Classified[?] => "Classified(***)"
    case other => other

  /** Render something that came out of a classified computation for the user.
    *
    * The rendering itself is a classified sink: an agent-defined `toString` (or
    * even a custom `Throwable.getMessage`) runs on the secret and can throw an
    * exception containing it, or a fatal error or control throwable that depends
    * on it. Letting either escape would reach agent code. Rendering failures are
    * therefore turned into user-only text, as `map` keeps them, and the call
    * returns normally; only the stop signals propagate. */
  private def classifiedUserText(value: Try[Any]): String =
    def errorText(error: Throwable): String =
      ClassifiedImpl
        .attempt(Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.toString))
        .getOrElse("(error details could not be rendered)")

    value match
      case Success(plain) =>
        ClassifiedImpl.attempt(Option(String.valueOf(plain)).getOrElse("null")) match
          case Success(text) => text
          case Failure(error) => s"<classified rendering failed: ${errorText(error)}>"
      case Failure(error) => s"<classified error: ${errorText(error)}>"

  private def userView(value: Any): Any = value match
    case classified: Classified[?] =>
      // Always return an already-rendered String. In particular, do not hand an
      // agent-defined object to `String.valueOf`/`Formatter` outside this guard.
      classifiedUserText(ClassifiedImpl.unwrap(classified))
    case other => other

  private def emit(value: Any, suffix: String = ""): Unit =
    output.print(String.valueOf(agentView(value)) + suffix, String.valueOf(userView(value)) + suffix)

  def println(value: Any)(using UserIO): Unit = emit(value, "\n")
  def println()(using UserIO): Unit = output.print("\n", "\n")
  def print(value: Any)(using UserIO): Unit = emit(value)
  def printf(format: String, args: Any*)(using UserIO): Unit =
    // Preserve the types of plain arguments for numeric and date conversions.
    output.print(format.format(args.map(agentView)*), format.format(args.map(userView)*))

  def ask(question: String)(using UserIO): Option[String] = ui.askUser(question, Nil, false)
  def ask(question: String, options: List[String])(using UserIO): Option[String] =
    ui.askUser(question, options, false)
  def ask(question: String, options: List[String], multiple: Boolean)(using UserIO): Option[String] =
    ui.askUser(question, options, multiple)

  def setTodos(items: List[Todo])(using UserIO): Unit = todoLock.synchronized:
    todoList = items
    ui.showTodos(items)

  def todos(using UserIO): List[Todo] = todoList

  def markTodo(text: String, status: TodoStatus)(using UserIO): Unit = todoLock.synchronized:
    if !todoList.exists(_.text == text) then
      throw IllegalArgumentException(s"No TODO item with text '$text'. Current: ${todoList.map(_.text).mkString(", ")}")
    setTodos(todoList.map(todo => if todo.text == text then todo.copy(status = status) else todo))

  private[atc] def currentTodos: List[Todo] = todoList

  private[atc] def clearTodos(): Unit = todoLock.synchronized:
    todoList = Nil
    notes = TaskNotes()

  def classify[T](value: T): Classified[T] = ClassifiedImpl.wrap(value)

  def chat(message: String)(using UserIO): String = llm.chat(message)
  def classifiedChat(message: String): String = llm.classifiedChat(message)
  def classifiedChat(message: Classified[String]): Classified[String] = message.map(classifiedChat)

private[atc] object HostInteraction:
  /** The most characters task notes may hold, counted over all their entries. */
  val MaxTaskNotesChars = 16000

  def length(notes: TaskNotes): Int =
    (notes.goal :: (notes.constraints ++ notes.completed ++ notes.remaining)).mkString("\n").length
