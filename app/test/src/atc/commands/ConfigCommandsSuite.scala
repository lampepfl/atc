package atc.commands

import atc.config.{Config, ProjectTrust}

import java.nio.file.Files

/** The settings `/config` changes: what a typed value means, and how the value in force reads. */
class ConfigCommandsSuite extends munit.FunSuite:
  import ConfigCommands.Setting.*

  test("typed values become config values, and anything else is refused with what is allowed"):
    assertEquals(PredictInput.parse("off"), Right(ujson.False))
    assertEquals(WebSearch.parse(" TRUE "), Right(ujson.True))
    assertEquals(Notifications.parse("Bell"), Right(ujson.Str("bell")))
    assertEquals(AutoCompactThreshold.parse("0.5"), Right(ujson.Num(0.5)))
    assertEquals(CompactKeepRatio.parse("0"), Right(ujson.Num(0)))
    assertEquals(PredictInput.parse("maybe"), Left("predictInput is true or false, not 'maybe'"))
    assertEquals(Notifications.parse("loud").left.map(_.contains("auto | system | terminal | bell | off")), Left(true))
    for bad <- List("1.5", "-0.1", "NaN", "Infinity", "half") do
      assert(AutoCompactThreshold.parse(bad).isLeft, bad)

  test("every offered choice parses, and the value in force reads as one"):
    for setting <- values; choice <- setting.choices do assert(setting.parse(choice).isRight, s"${setting.key} $choice")
    val defaults = Config()
    for setting <- values do assert(setting.choices.contains(setting.current(defaults)), setting.key)
    assertEquals(AutoCompactThreshold.current(Config(autoCompactThreshold = 0.75)), "0.75")

  test("settings are named case-insensitively; policy settings are not among them"):
    assertEquals(named("AUTOCOMPACTTHRESHOLD"), Some(AutoCompactThreshold))
    assertEquals(named("maxToolCalls"), None)

  test("a config edit keeps a trusted project trusted, and a failed one restores the files and the trust"):
    val atcDir = Files.createTempDirectory("atc-home").nn
    def project(trusted: Boolean) =
      val root = Files.createTempDirectory("atc-project").nn
      Files.createDirectories(root.resolve(".atc"))
      Files.writeString(Config.projectPath(root), """{ "commands": ["ls"] }""")
      if trusted then ProjectTrust.trust(root, atcDir)
      root
    def webSearch(root: java.nio.file.Path) = Config.setTopLevel(Config.projectPath(root), "webSearch", ujson.True)
    val trusted = project(trusted = true)
    ConfigCommands.editConfigs(trusted, atcDir, List(Config.projectPath(trusted)))(webSearch(trusted))(())
    assertEquals(ProjectTrust.pending(trusted, atcDir), None, "the user's own edit keeps it trusted")
    val untrusted = project(trusted = false)
    ConfigCommands.editConfigs(untrusted, atcDir, List(Config.projectPath(untrusted)))(webSearch(untrusted))(())
    assert(ProjectTrust.pending(untrusted, atcDir).isDefined, "and does not trust one that was not")
    val config = Config.projectPath(trusted)
    val before = Files.readString(config)
    val global = atcDir.resolve("config.json").nn
    intercept[IllegalStateException]:
      ConfigCommands.editConfigs(trusted, atcDir, List(config, global)) {
        Config.setTopLevel(config, "hosts", ujson.Arr("example.com"))
        Config.setTopLevel(global, "model", ujson.Str("m"))
      }(throw IllegalStateException("the reload failed"))
    assertEquals(Files.readString(config), before)
    assert(!Files.exists(global), "the global config it created is gone")
    assertEquals(ProjectTrust.pending(trusted, atcDir), None)
