package atc.ui

import atc.perms.{Decision, PermissionRequest}

import java.util.Locale
import scala.annotation.tailrec

import Ansi.{Bold, Cyan, Dim, Green, Red, Yellow}

/** What the pop-ups show and read: permission requests, questions, confirmations, secrets
  * and choices. Every method must run inside `Tui.popupBlock`, which makes it a block of
  * its own and pauses the turn's key reader for it. Menus and answer fields show their keys
  * in the footer; no screen lock is held while they read. A menu gives indices, so duplicate
  * labels stay apart, and `None` when left with Esc, Ctrl-C or Ctrl-D; `escape` names where
  * Esc goes in the footer: `back` in a slash command's menus, `cancel` in the agent's pop-ups. */
private[ui] final class Dialogs(screen: Screen, alerts: Alerts, status: StatusLine, prompt: PromptReader):
  import screen.{Indent, g, plain, styled, width, write}

  private val menus = ListMenu(screen, status, alerts)

  /** A single-choice menu, its cursor on `initial` at first: the index of the chosen label. */
  def menuIndex(message: String, labels: List[String], escape: String, initial: Int): Option[Int] =
    menus.run(message, labels, multi = false, initial, Set.empty, escape).flatMap(_.headOption)

  /** A multi-choice menu with the `checked` options ticked at first: the ticked indices. */
  def checkboxIndices(message: String, labels: List[String], checked: Set[Int], escape: String): Option[List[Int]] =
    menus.run(message, labels, multi = true, 0, checked, escape)

  /** Where a pop-up continues: under the text of its question or request. */
  private val Under = Indent + Indent

  /** A pop-up's menu, its cursor on `initial` at first: under its question, which it does
    * not repeat as a title. */
  private def menu(labels: List[String], escape: String, initial: Int = 0): Option[String] =
    menus.run("", labels, multi = false, initial, Set.empty, escape, Under).flatMap(_.headOption).flatMap(labels.lift)

  private def note(text: String): Unit = write(Under + styled(text, Dim) + "\n")

  /** A question, wrapped with its lines under the first one's text. */
  private def ask(question: String): Unit =
    TextLayout.wrap(Ansi.sanitize(question), width - 5, Under.length).zipWithIndex.foreach: (line, index) =>
      write(Indent + styled((if index == 0 then "? " else "  ") + line, Cyan, Bold) + "\n")

  /** A permission request: allow once, allow for the session, deny, or instructions for the
    * agent. A cancelled menu denies, so Esc is shown as deny; empty or cancelled instructions
    * return to the menu. A plain terminal reads a typed reply ([[Menus.permissionReply]]). */
  def permission(req: PermissionRequest): Decision =
    // The request embeds model-chosen paths and command lines: sanitize.
    write(Indent + styled(s"${g.warn} Permission request: ${Ansi.sanitize(req.title)}", Yellow, Bold) + "\n")
    req.details.foreach: detail =>
      TextLayout.wrap(Ansi.sanitize(detail), width - 5, Under.length)
        .foreach(line => write(Indent + Indent + styled(line, Yellow) + "\n"))
    val choices = List(Menus.AllowOnce, Menus.AllowSession, Menus.DenyLabel, Menus.ReviseLabel)
    /** The decision, `None` when the menu is cancelled. After empty instructions the menu
      * opens again on that row, never on Allow once. */
    @tailrec def choose(initial: Int): Option[Decision] =
      menu(choices, escape = "deny", initial) match
        case Some(Menus.AllowOnce) => Some(Decision.AllowOnce)
        case Some(Menus.AllowSession) => Some(Decision.AllowSession)
        case Some(Menus.ReviseLabel) =>
          note("Describe what to change. The current request will not be approved.")
          freeText(Under + styled("instructions> ", Cyan)) match
            case Some(text) => Some(Decision.Revise(text))
            case None => choose(choices.indexOf(Menus.ReviseLabel))
        case Some(_) => Some(Decision.Deny)
        case None => None
    val chosen =
      if !plain then choose(0)
      else
        val reply = freeText(styled("Allow? [y]es once / [s]ession / [n]o / type instructions: ", Yellow))
        Some(Menus.permissionReply(reply))
    val decision = chosen.getOrElse(Decision.Deny)
    // The menu echoes the choice and the instructions stay where they were typed; confirm
    // only what the user did not see.
    decision match
      case Decision.Revise(_) => write(Under + styled(s"${g.arrow} sent to the agent", Cyan) + "\n")
      case _ if plain || chosen.isEmpty =>
        val label = decision match
          case Decision.AllowOnce => styled(s"${g.arrow} allowed once", Green)
          case Decision.AllowSession => styled(s"${g.arrow} allowed for this session", Green)
          case _ => styled(s"${g.arrow} denied", Red)
        write(Under + label + "\n")
      case _ => ()
    decision

  /** A yes/no question from the app itself (setup, not the agent): a menu when there is a
    * terminal, a `[y/N]` line otherwise, where only `y` or `yes` means yes. Cancelling means no. */
  def confirm(question: String): Boolean =
    ask(question)
    val yes =
      if plain then freeText(Under + styled("[y/N]: ", Cyan)).exists(a => Set("y", "yes")(a.toLowerCase(Locale.ROOT)))
      else menu(List(Menus.YesLabel, Menus.NoLabel), escape = "cancel").contains(Menus.YesLabel)
    if plain then
      write(Under + styled(s"${g.arrow} ${if yes then "yes" else "no"}", if yes then Green else Red) + "\n")
    yes

  /** A secret such as an API key: the typed text is shown as `*` and never enters the
    * prompt history. `None` when empty or cancelled. */
  def secret(question: String): Option[String] =
    ask(question)
    answerField(prompt.readSecret(Under + styled("key> ", Cyan)))

  /** A question from the agent. Options render as a menu (or checkboxes when `multiple`),
    * always with a custom-answer entry; no options give a free-text line. `None` on
    * Ctrl-C/Ctrl-D. */
  def answer(question: String, options: List[String], multiple: Boolean): Option[String] =
    // The question and options are model-written: sanitize.
    ask(question)
    val cleanOptions = options.map(Ansi.sanitize)
    val answerPrompt = Under + styled("answer> ", Cyan)
    val reply =
      if cleanOptions.isEmpty || plain then
        cleanOptions.foreach(o => write(Under + styled(s"- $o", Cyan) + "\n"))
        if cleanOptions.nonEmpty then note("Choose a listed answer or type your own answer or instructions.")
        freeText(answerPrompt)
      else if multiple then
        val labels = cleanOptions :+ Menus.AddAnswerLabel
        menus.run("", labels, multi = true, 0, Set.empty, "cancel", Under).flatMap: ids =>
          val chosen = ids.sorted.flatMap(options.lift)
          if ids.contains(cleanOptions.size) then freeText(answerPrompt).map(t => (chosen :+ t).mkString("; "))
          else Option.when(chosen.nonEmpty)(chosen.mkString("; "))
      else
        val labels = cleanOptions :+ Menus.OtherLabel
        menus.run("", labels, multi = false, 0, Set.empty, "cancel", Under).flatMap(_.headOption).flatMap: i =>
          if i == cleanOptions.size then freeText(answerPrompt) else options.lift(i)
    // A single-choice menu echoes the selection itself; confirm the other outcomes.
    reply match
      case Some(a) if options.isEmpty || plain || multiple || !options.contains(a) =>
        write(Under + styled(s"${g.arrow} ${Ansi.sanitize(a)}", Green) + "\n")
      case Some(_) => ()
      case None => write(Under + styled(s"${g.arrow} No answer", Dim) + "\n")
    reply

  private def freeText(promptText: String): Option[String] = answerField(prompt.readAnswer(promptText))

  /** Read typed text with the field's keys in the footer. */
  private def answerField(read: => Option[String]): Option[String] =
    status.withHint(s"Enter send ${g.dot} Ctrl-C cancel"):
      screen.synchronized(screen.flush())
      try read
      finally screen.tail = "\n"
