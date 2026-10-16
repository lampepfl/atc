package atc.ui

import atc.perms.Decision

import java.util.Locale

/** The rows of the pop-ups' and slash commands' menus, and the typed reply to a permission
  * request on a plain terminal. */
private[atc] object Menus:
  val AllowOnce = "Allow once"
  val AllowSession = "Allow for this session"
  val DenyLabel = "Deny this request"
  val ReviseLabel = "Tell the agent what to change"
  val OtherLabel = "Write a different answer"
  val AddAnswerLabel = "Add an answer or instructions"
  val YesLabel = "Yes"
  val NoLabel = "No"
  /** The last row of a menu the user comes back to. */
  val DoneLabel = "Done"
  /** The last row of a sub-menu: back to the menu that opened it. */
  val BackLabel = "Back"

  /** Plain permission prompts accept exact approvals; every other answer is feedback. */
  private[atc] def permissionReply(answer: Option[String]): Decision =
    answer.map(_.trim).filter(_.nonEmpty) match
      case None => Decision.Deny
      case Some(text) => text.toLowerCase(Locale.ROOT) match
          case "y" | "yes" => Decision.AllowOnce
          case "s" | "session" => Decision.AllowSession
          case "n" | "no" => Decision.Deny
          case _ => Decision.Revise(text)
