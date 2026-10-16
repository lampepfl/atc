package atc.commands

/** The slash-command table: what `/help` prints, what Tab offers, and how a
  * typed line resolves (names, aliases, arguments). */
class SlashCommandSuite extends munit.FunSuite:
  import SlashCommand.*

  test("every name and alias is unique, and names are offered for completion in help order"):
    val spellings = values.toList.flatMap(c => c.name :: c.aliases.toList)
    assertEquals(spellings.distinct, spellings)
    assertEquals(names.head, "/help")
    assertEquals(names.last, "/quit")
    assert(names.forall(_.startsWith("/")))

  test("an alias that starts a name belongs to that command (Enter runs the prompt's selected name)"):
    for command <- values; alias <- command.aliases; other <- values if other.name.startsWith(alias) do
      assertEquals(other, command, alias)

  test("parse resolves names and aliases case-insensitively and splits off the argument"):
    assertEquals(parse("/compact preserve paths"), Right((Compact, "preserve paths")))
    assertEquals(parse("/help"), Right((Help, "")))
    assertEquals(parse("/?"), Right((Help, "")))
    assertEquals(parse("/Q"), Right((Quit, "")))
    assertEquals(parse("  /model   gpt-5  "), Right((Model, "gpt-5")))
    assertEquals(parse("/classified off"), Right((ClassifiedModel, "off")))
    assertEquals(parse("/mode read only"), Right((Mode, "read only")))
    // Code keeps its case and its newlines (a pasted block).
    assertEquals(parse("/run val X = 1\nX + 1"), Right((Run, "val X = 1\nX + 1")))
    assertEquals(parse("/scala"), Right((Run, "")))

  test("a line starting with an absolute path is a request; a mistyped command is still a command"):
    assert(!isCommandLine("/Users/me/project/Main.scala fails to compile, fix it"))
    assert(!isCommandLine("  /etc/hosts"))
    assert(isCommandLine("/hlep"))
    assert(isCommandLine(" /model gpt-5"))
    assert(isCommandLine("/save ~/notes/session.json"))
    assert(isCommandLine("/run val ratio = a / b"))
    assert(!isCommandLine("fix /tmp/x"))

  test("a command's arguments complete under its aliases and in any case"):
    def complete(words: String*) = Commands.complete(words.toList, List("a", "p/b"), List("low", "high"))
    assertEquals(complete("/mo"), names)
    assertEquals(complete("/classified", ""), List("none", "a", "p/b"))
    assertEquals(complete("/MODEL", "p"), List("a", "p/b"))
    assertEquals(complete("/effort", ""), List("low", "high"))
    assertEquals(complete("/permissions", ""), List("revoke"))
    assertEquals(complete("/config", "notifications", ""), List("auto", "system", "terminal", "bell", "off"))
    assertEquals(complete("/model", "a", ""), Nil)
    assertEquals(complete("/bogus", ""), Nil)

  test("parse reports an unknown command by what was typed"):
    assertEquals(parse("/bogus 1 2"), Left("/bogus"))
    assertEquals(parse("/helpme"), Left("/helpme"))
