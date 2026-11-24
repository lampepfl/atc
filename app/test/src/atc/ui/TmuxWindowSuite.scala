package atc.ui

import scala.collection.mutable

/** The name atc gives the tmux window it runs in. */
class TmuxWindowSuite extends munit.FunSuite:
  /** A window of pane `%3` on a tmux that answers with `naming` (`#{automatic-rename}`) and
    * `own` (the window's own setting), and the commands it was sent. */
  private def window(naming: Option[String], own: String = ""): (TmuxWindow, mutable.Buffer[String]) =
    val sent = mutable.Buffer[String]()
    val run: List[String] => Option[String] =
      case "display-message" :: _ => naming
      case "show-options" :: _ => Some(own + "\n")
      case command =>
        sent += command.mkString(" ")
        Some("")
    (TmuxWindow("%3", run, _.run()), sent)

  test("a window tmux names gets the title, and is handed back as it was"):
    val (tmux, sent) = window(naming = Some("1\n"))
    tmux.rename("atc · project")
    tmux.rename("● atc · project")
    tmux.restore()
    assertEquals(
      sent.toList,
      List(
        "rename-window -t %3 -- atc · project",
        "rename-window -t %3 -- ● atc · project",
        "set-option -w -t %3 -u automatic-rename",
      ),
    )

  test("a window with a setting of its own gets it back, not the session's"):
    val (tmux, sent) = window(naming = Some("1"), own = "on")
    tmux.rename("atc")
    tmux.restore()
    assertEquals(sent.last, "set-option -w -t %3 automatic-rename on")

  test("a window the user named keeps its name"):
    val (tmux, sent) = window(naming = Some("0\n"), own = "off")
    tmux.rename("atc · project")
    tmux.restore()
    assertEquals(sent.toList, Nil)

  test("a tmux that cannot be asked is left alone"):
    val (tmux, sent) = window(naming = None)
    tmux.rename("atc · project")
    tmux.restore()
    assertEquals(sent.toList, Nil)

  test("nothing is restored that was not named, and nothing named after the restore"):
    val (tmux, sent) = window(naming = Some("1"))
    tmux.restore()
    tmux.rename("atc · project")
    tmux.restore()
    assertEquals(sent.toList, Nil)

  test("outside tmux there is no window"):
    assertEquals(TmuxWindow.current(_ => None), None)
    assertEquals(TmuxWindow.current(Map("TMUX" -> "/tmp/tmux-1000/default,1,0").get), None)
    assertEquals(TmuxWindow.current(Map("TMUX" -> "", "TMUX_PANE" -> "%1").get), None)
