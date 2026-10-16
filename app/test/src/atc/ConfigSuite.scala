package atc

import atc.config.*
import atc.perms.{Access, Decision, GlobMatcher, Mode, Policy}

import java.nio.charset.CharacterCodingException
import java.nio.file.{Files, Path}

/** ATC's configuration: JSON parsing, layered merge, file-rule construction,
  * access-level parsing and API-key resolution. (ATC has no scopt CLI config
  * like TACIT; this covers the equivalent surface.) */
class ConfigSuite extends munit.FunSuite:
  /** Load with an absent global layer, so a test never depends on the machine's
    * own `~/.atc/config.json`. Layer combination and project trust are `LayerSuite`. */
  private def load(dir: Path, explicit: Option[Path]): Configuration =
    Config.load(dir, explicit, dir.resolve("no-such-global.json").nn, trustProject = true)

  private def writeCfg(dir: Path, name: String, json: String): Path =
    val p = dir.resolve(name)
    Files.writeString(p, json)
    p

  // ── Access.parse ────────────────────────────────────────────────

  test("Access.parse accepts the documented spellings"):
    assertEquals(Access.parse("none"), Access.None)
    assertEquals(Access.parse("deny"), Access.None)
    assertEquals(Access.parse("read"), Access.Read)
    assertEquals(Access.parse("r"), Access.Read)
    assertEquals(Access.parse("ro"), Access.Read)
    assertEquals(Access.parse("write"), Access.Write)
    assertEquals(Access.parse("rw"), Access.Write)
    assertEquals(Access.parse("read-write"), Access.Write)
    assertEquals(Access.parse("  WRITE "), Access.Write)
  test("Access.parse rejects an unknown level"):
    val e = intercept[IllegalArgumentException](Access.parse("execute"))
    assert(e.getMessage.nn.contains("execute"), e.getMessage)

  // ── defaults ────────────────────────────────────────────────────

  test("an empty config has the documented defaults"):
    val c = Config()
    assertEquals(c.model, None)
    assertEquals(c.safeMode, true)
    assertEquals(c.executionTimeoutMs, Some(300000L))
    assertEquals(c.maxToolCalls, 200)
    assertEquals(c.maxToolOutputChars, 40000)
    assert(c.files.isEmpty && c.commands.isEmpty && c.hosts.isEmpty)
    assert(c.denyCommands.isEmpty && c.denyHosts.isEmpty)
    assertEquals(c.respectGitignore, true)

  test("the starting global config protects the usual secrets and its own directory"):
    // Asserted as properties, not as a list: the template is meant to be edited.
    val start = upickle.default.read[Config](ujson.read(Config.globalTemplate))
    val classified = start.files.filter(_.classified.contains(true)).map(_.path)
    for expected <- List(".ssh", ".env", ".env.*", "*.pem", "id_rsa", ".aws") do
      assert(classified.contains(expected), s"missing $expected")
    val ownConfig = start.files.find(_.path == ".atc").get
    assertEquals(ownConfig.access, Some("none"))
    assert(ownConfig.locked, "the config that grants everything must not be reachable")
    assert(start.commands.isEmpty, "a fresh config grants no command")
    for command <- List("rm -rf /tmp/example", "sudo", "sudo -s") do
      assert(start.denyCommands.exists(GlobMatcher.matchesCommand(command, _)), s"must deny `$command`")

  test("the starting global config denies common command shells"):
    val denied = upickle.default.read[Config](ujson.read(Config.globalTemplate)).denyCommands
    val shellInvocations = List(
      "sh" -> "sh -c true",
      "bash" -> "bash -c true",
      "dash" -> "dash -c true",
      "ash" -> "ash -c true",
      "zsh" -> "zsh -c true",
      "ksh" -> "ksh -c true",
      "ksh93" -> "ksh93 -c true",
      "mksh" -> "mksh -c true",
      "fish" -> "fish -c true",
      "csh" -> "csh -c true",
      "tcsh" -> "tcsh -c true",
      "cmd" -> "cmd /c echo ok",
      "powershell" -> "powershell -Command Write-Output ok",
      "pwsh" -> "pwsh -Command Write-Output ok",
      "wsl" -> "wsl -e sh -c true",
      "git-bash" -> "git-bash --cd-to-home",
    )
    for (bare, withArgs) <- shellInvocations do
      assert(denied.contains(bare), s"the starting denyCommands must contain the bare shell name `$bare`")
      for command <- List(bare, withArgs) do
        assert(
          denied.exists(GlobMatcher.matchesCommand(command, _)),
          s"the starting denyCommands must refuse shell invocation `$command`: ${denied.mkString(", ")}",
        )

  test("a provider may list no models: an endpoint whose models are listed or filled in later"):
    val dir = Files.createTempDirectory("atc-cfg-empty-provider").nn
    val cfg = writeCfg(
      dir,
      "config.json",
      """{ "providers": { "openrouter": { "api": "openai", "url": "https://openrouter.ai/api/v1" } } }"""
    )
    val loaded = load(dir, Some(cfg))
    assertEquals(loaded.settings.providers("openrouter").models, Map.empty[String, ModelConfig])
    val catalog = ModelCatalog.from(loaded.settings)
    assertEquals(catalog.configured, Nil, "it configures no model")
    assertEquals(catalog.discoverable.map(_.provider), List("openrouter"), "it lists its own")
    // and a later layer can add one without repeating the endpoint
    val withModel = upickle.default.read[Config](Configuration.mergeJson(
      ujson.read(Files.readString(cfg)).obj,
      ujson.read(
        """{ "providers": { "openrouter": { "models": { "sonnet": { "name": "anthropic/claude-sonnet-4.5" } } } } }"""
      ).obj
    ))
    val spec = ModelCatalog.from(withModel).find("sonnet")
    assertEquals(spec.modelId, "anthropic/claude-sonnet-4.5") // a model *name* may contain a slash
    assertEquals(spec.baseUrl, Some("https://openrouter.ai/api/v1"))

  test("the starting global config is a usable set of providers"):
    val start = upickle.default.read[Config](ujson.read(Config.globalTemplate))
    assert(start.providers.nonEmpty)
    assert(start.providers.forall((_, p) => p.api.exists(_.nonEmpty)), start.toString)
    assert(start.providers.forall((_, p) => p.models.isEmpty), "each provider lists its own models")
    assertEquals(start.model, None, "no model is named before one is listed")
    assertEquals(start.classifiedModel, None, "a classified model is chosen deliberately")

  // ── parsing a single file ───────────────────────────────────────

  test("a full config parses into the model"):
    val dir = Files.createTempDirectory("atc-cfg").nn
    val cfg = writeCfg(
      dir,
      "config.json",
      """
      {
        "model": "claude", "classifiedModel": "local",
        "providers": {
          "anthropic": { "api": "anthropic",
            "models": { "claude": { "name": "claude-opus-5", "webSearch": true } } },
          "ollama": { "api": "openai", "url": "http://localhost:11434/v1",
            "models": { "local": {} } }
        },
        "files": [ { "path": ".", "access": "write" }, { "path": "secrets", "classified": true } ],
        "commands": ["git status"], "hosts": ["*.scala-lang.org"],
        "safeMode": false, "maxToolCalls": 10
      }
    """
    )
    val loaded = load(dir, Some(cfg))
    val c = loaded.settings
    assertEquals(loaded.layers.flatMap(_.path), List(cfg))
    assertEquals(c.model, Some("claude"))
    assertEquals(c.classifiedModel, Some("local"))
    assertEquals(c.providers("anthropic").models("claude").name, Some("claude-opus-5"))
    // `name` defaults to the alias, and provider settings reach the resolved model
    val local = ModelCatalog.from(c).find("local")
    assertEquals(local.modelId, "local")
    assertEquals(local.api, "openai")
    assertEquals(local.baseUrl, Some("http://localhost:11434/v1"))
    assertEquals(local.apiKey, None) // keys live in keys.properties, never in a config
    // the file's own rules come after the built-in layer's
    assertEquals(loaded.rules.takeRight(2).map(_.rule.path), List(".", "secrets"))
    assertEquals(loaded.rules.last.rule.classified, Some(true))
    assertEquals(c.commands, List("git status"))
    assertEquals(c.safeMode, false)
    assertEquals(c.maxToolCalls, 10)

  test("UTF-8 BOMs from Windows editors are accepted and preserved when editing"):
    val dir = Files.createTempDirectory("atc-cfg-bom").nn
    val cfg = writeCfg(dir, "config.json", "\uFEFF{\r\n  \"safeMode\": true\r\n}\r\n")
    assert(load(dir, Some(cfg)).settings.safeMode)
    Config.setTopLevel(cfg, "safeMode", ujson.Bool(false))
    val edited = Files.readString(cfg).nn
    assert(edited.startsWith("\uFEFF{\r\n"), edited)
    assertEquals(ujson.read(edited.stripPrefix("\uFEFF"))("safeMode").bool, false)

  test("a reasoning style fills in the effort, or leaves its member out without one"):
    val fragment = ujson.read("""{ "extra_body": { "google": { "thinking_config":
      { "include_thoughts": true, "thinking_level": "{effort}" } } } }""")
    assertEquals(
      ReasoningStyle.withEffort(fragment, Some("high"))("extra_body")("google")("thinking_config"),
      ujson.Obj("include_thoughts" -> true, "thinking_level" -> "high"),
    )
    assertEquals(
      ReasoningStyle.withEffort(fragment, None)("extra_body")("google")("thinking_config"),
      ujson.Obj("include_thoughts" -> true),
    )
    val gemini = upickle.default.read[Config](ujson.read(Config.globalTemplate)).providers("gemini")
    assertEquals(gemini.reasoningStyle.flatMap(_.tags), Some(List("<thought>", "</thought>")))

  test("a reasoning style is checked: Chat Completions only, JSON objects, two tags"):
    val dir = Files.createTempDirectory("atc-cfg-style").nn
    def problem(provider: String) =
      val file = writeCfg(dir, "config.json", s"""{ "providers": { "p": $provider } }""")
      intercept[IllegalArgumentException](load(dir, Some(file))).getMessage.nn
    assert(problem("""{ "api": "anthropic", "reasoningStyle": { "tags": ["<t>", "</t>"] } }""").contains("api openai"))
    assert(
      problem("""{ "api": "openai", "reasoningStyle": { "request": [1] } }""").contains("request must be a JSON object")
    )
    assert(problem("""{ "api": "openai", "reasoningStyle": { "tags": ["<t>"] } }""").contains("two non-empty strings"))

  test("a malformed config file is a clear error"):
    val dir = Files.createTempDirectory("atc-cfg-bad").nn
    val bad = writeCfg(dir, "config.json", "{ not json ]")
    val e = intercept[IllegalArgumentException](load(dir, Some(bad)))
    assert(e.getMessage.nn.contains("Cannot parse config"), e.getMessage)

  test("a non-object config file is rejected"):
    val dir = Files.createTempDirectory("atc-cfg-arr").nn
    val arr = writeCfg(dir, "config.json", "[1, 2, 3]")
    val e = intercept[IllegalArgumentException](load(dir, Some(arr)))
    assert(e.getMessage.nn.contains("must be a JSON object"), e.getMessage)

  test("invalid runtime limits are rejected while loading config"):
    val dir = Files.createTempDirectory("atc-cfg-limits").nn
    val bad = writeCfg(dir, "config.json", """{ "maxToolOutputChars": 0 }""")
    val e = intercept[IllegalArgumentException](load(dir, Some(bad)))
    assert(e.getMessage.nn.contains("maxToolOutputChars"), e.getMessage)

  test("a null for a setting that is not optional, or a fraction for a whole number, is rejected with its file"):
    val dir = Files.createTempDirectory("atc-cfg-exact").nn
    def rejected(json: String): String =
      val cfg = writeCfg(dir, "config.json", json)
      val e = intercept[IllegalArgumentException](load(dir, Some(cfg)))
      assert(e.getMessage.nn.startsWith(s"Invalid config $cfg: "), e.getMessage)
      e.getMessage.nn
    for key <- List("files", "denyCommands", "commands", "providers", "notifications", "safeMode", "maxToolCalls") do
      val message = rejected(s"""{ "$key": null }""")
      assert(message.endsWith(s": $key must not be null"), message)
    def model(settings: String) = s"""{ "providers": { "p": { "api": "echo", "models": { "m": $settings } } } }"""
    assert(rejected("""{ "providers": { "p": null } }""").endsWith("providers.p must not be null"))
    assert(
      rejected("""{ "providers": { "p": { "api": "echo", "models": null } } }""").endsWith("models must not be null")
    )
    assert(rejected(model("null")).endsWith("providers.p.models.m must not be null"))
    assert(rejected(model("""{ "efforts": ["low", null] }""")).endsWith("m.efforts[1] must not be null"))
    assert(
      rejected("""{ "files": [ { "path": ".", "locked": null } ] }""").endsWith("files[0].locked must not be null")
    )
    assert(rejected("""{ "maxToolCalls": 1e10 }""").endsWith("in range (was 10000000000)"))
    assert(rejected("""{ "maxToolOutputChars": 1.5 }""").endsWith("must be a whole number in range (was 1.5)"))
    assert(rejected("""{ "executionTimeoutMs": 1e30 }""").contains("executionTimeoutMs must be a whole number"))
    assert(rejected(model("""{ "maxTokens": 2.5 }""")).contains("providers.p.models.m.maxTokens must be a whole"))
    // A null unsets an optional setting, and a reasoning style's request is JSON taken as written.
    val ok = writeCfg(
      dir,
      "ok.json",
      """{ "executionTimeoutMs": null, "classifiedModel": null, "maxToolCalls": 1e3,
        |  "providers": { "p": { "api": "openai", "reasoningStyle": { "request": { "x": null } },
        |                         "models": { "m": { "maxTokens": 4096.0 } } } } }""".stripMargin
    )
    val settings = load(dir, Some(ok)).settings
    assertEquals(settings.executionTimeoutMs, None)
    assertEquals(settings.maxToolCalls, 1000)
    assertEquals(settings.providers("p").models("m").maxTokens, Some(4096))

  test("a files entry with an unknown access level or an unusable pattern is rejected while loading"):
    val dir = Files.createTempDirectory("atc-cfg-rules").nn
    val badAccess = writeCfg(dir, "access.json", """{ "files": [ { "path": ".", "access": "rw2" } ] }""")
    val e = intercept[IllegalArgumentException](load(dir, Some(badAccess)))
    assert(e.getMessage.nn.contains("Invalid config") && e.getMessage.nn.contains("rw2"), e.getMessage)
    val badGlob = writeCfg(dir, "glob.json", """{ "files": [ { "path": "src/[a", "access": "read" } ] }""")
    val g = intercept[IllegalArgumentException](load(dir, Some(badGlob)))
    assert(g.getMessage.nn.contains("Invalid config") && g.getMessage.nn.contains("src/[a"), g.getMessage)
    val blank = writeCfg(dir, "blank.json", """{ "files": [ { "path": "  " } ] }""")
    intercept[IllegalArgumentException](load(dir, Some(blank)))

  test("with no config file at all, nothing is configured and nothing is permitted"):
    val dir = Files.createTempDirectory("atc-cfg-none").nn
    val loaded = load(dir, None)
    assert(loaded.layers.flatMap(_.path).isEmpty)
    assertEquals(loaded.layers, Nil)
    assertEquals(loaded.rules, Nil)
    assertEquals(loaded.settings.commands, Nil)

  test("the same config path is not merged twice"):
    val dir = Files.createTempDirectory("atc-cfg-dedup").nn
    val projectDir = Files.createDirectories(dir.resolve(".atc")).nn
    val project = writeCfg(projectDir, "config.json", """{ "commands": ["git status"] }""")
    val loaded = load(dir, Some(project))
    assertEquals(loaded.layers.flatMap(_.path), List(project))
    // read once, in its project role
    assertEquals(loaded.layers.map(_.origin), List(Origin.Project))
    assertEquals(loaded.settings.commands, List("git status"))

  // ── layered merge (global ← project ← explicit) ─────────────────

  test("later layers override scalars, extend list keys and merge models"):
    val dir = Files.createTempDirectory("atc-cfg-merge").nn
    val base = writeCfg(
      dir,
      "a.json",
      """
      { "model": "a", "safeMode": true, "files": [ {"path": "."} ], "commands": ["ls"],
        "providers": { "anthropic": { "api": "anthropic", "models": { "a": { "name": "m1" } } } } }
    """
    )
    val over = writeCfg(
      dir,
      "b.json",
      """
      { "model": "b", "safeMode": false, "files": [ {"path": "secrets", "classified": true} ],
        "commands": ["git status"],
        "providers": { "openai": { "api": "openai-responses", "models": { "b": { "name": "m2" } } } } }
    """
    )
    // load merges global(a) ← project ← explicit(b); this drives mergeJson directly.
    val merged = Configuration.mergeJson(ujson.read(Files.readString(base)).obj, ujson.read(Files.readString(over)).obj)
    val c = upickle.default.read[Config](merged)
    assertEquals(c.model, Some("b")) // scalar overridden
    assertEquals(c.safeMode, false) // scalar overridden
    assertEquals(c.files.map(_.path), List(".", "secrets")) // list extended
    assertEquals(c.commands, List("ls", "git status")) // list extended
    assertEquals(c.providers.keySet, Set("anthropic", "openai")) // providers merged
    assertEquals(ModelCatalog.from(c).labels, List("a", "b"))

  test("deny lists extend across layers, so a later layer cannot drop a deny pattern"):
    val a = ujson.read("""{ "commands": ["ls"], "denyCommands": ["rm *"], "denyHosts": ["*.internal"] }""").obj
    val b = ujson.read("""{ "commands": ["cat"], "denyCommands": ["curl *"] }""").obj
    val c = upickle.default.read[Config](Configuration.mergeJson(a, b))
    assertEquals(c.commands, List("ls", "cat"))
    assertEquals(c.denyCommands, List("rm *", "curl *"))
    assertEquals(c.denyHosts, List("*.internal"))

  test("a provider is merged field by field: models are added, a redefined alias replaced"):
    val a = ujson.read("""{ "providers": {
        "openai": { "api": "openai", "url": "http://old", "key": "k",
          "models": { "old": { "name": "v1", "webSearch": true } } },
        "anthropic": { "api": "anthropic", "models": { "claude": {} } } } }""").obj
    val b = ujson.read("""{ "providers": {
        "openai": { "api": "openai", "url": "http://new",
          "models": { "old": { "name": "v2" }, "fresh": { "name": "v3" } } } } }""").obj
    val c = upickle.default.read[Config](Configuration.mergeJson(a, b))
    val p = c.providers("openai")
    assertEquals(p.url, Some("http://new")) // provider scalar overridden
    assertEquals(p.models.keySet, Set("old", "fresh")) // model added, not replaced wholesale
    assertEquals(p.models("old").name, Some("v2")) // redefined alias replaced entirely
    assertEquals(p.models("old").webSearch, None) // ... including its dropped settings
    assertEquals(c.providers("anthropic").models.keySet, Set("claude")) // other providers untouched

  // ── API-key resolution ──────────────────────────────────────────

  test("a BOM-prefixed keys file is a properties file, and an empty value is not a binding"):
    val unset = "ATC_TEST_KEY_" + ProcessHandle.current().pid()
    val dir = Files.createTempDirectory("atc-keys").nn
    val file = dir.resolve(Config.KeysFile).nn
    Files.writeString(
      file,
      s"""|\uFEFF# a comment
          |! also a comment
          |
          |DEEPSEEK_API_KEY=sk-secret
          |COLON_KEY: colon value
          |SPACED_KEY = spaced
          |ESCAPED_KEY=with\\=sign
          |BLANK_KEY=
          |""".stripMargin
    )
    val keys = KeyBindings.load(List(file))
    assertEquals(keys.get("DEEPSEEK_API_KEY"), Some("sk-secret"))
    assertEquals(keys.get("COLON_KEY"), Some("colon value")) // `:` separates too, as in any .properties
    assertEquals(keys.get("SPACED_KEY"), Some("spaced"))
    assertEquals(keys.get("ESCAPED_KEY"), Some("with=sign"))
    assertEquals(keys.get("BLANK_KEY"), None) // an empty value means "not set"
    assertEquals(keys.get(unset), None)
    // the blank one is not listed
    assertEquals(keys.names, List("COLON_KEY", "DEEPSEEK_API_KEY", "ESCAPED_KEY", "SPACED_KEY"))

  test("the warning about a keys file other users can read shows its path without terminal controls"):
    assume(!atc.platform.Platform.isWindows, "POSIX permissions")
    val dir = Files.createDirectories(Files.createTempDirectory("atc-keys-shared").nn.resolve("a\u001b]0;x\u0007b")).nn
    val file = dir.resolve(Config.KeysFile).nn
    Files.writeString(file, "SHARED_KEY=value\n")
    Files.setPosixFilePermissions(file, java.nio.file.attribute.PosixFilePermissions.fromString("rw-r--r--"))
    val captured = java.io.ByteArrayOutputStream()
    val stderr = System.err
    System.setErr(java.io.PrintStream(captured, true, "UTF-8"))
    val keys =
      try KeyBindings.load(List(file))
      finally System.setErr(stderr)
    assertEquals(keys.get("SHARED_KEY"), Some("value"))
    val warning = captured.toString("UTF-8")
    assert(warning.contains("a]0;xb") && warning.contains("can be read by other users"), warning)
    assert(!warning.exists(c => c == '\u001b' || c == '\u0007'), warning)

  test("a ${VAR} in a provider's key is resolved through the bindings, then the environment"):
    val dir = Files.createTempDirectory("atc-keys-provider").nn
    val file = dir.resolve(Config.KeysFile).nn
    Files.writeString(file, "DEEPSEEK_API_KEY=sk-secret\n")
    val keys = KeyBindings.load(List(file))
    def spec(p: ProviderConfig) =
      ModelCatalog.from(Config(providers = Map("deepseek" -> p)), keys).find("m")
    val models = Map("m" -> ModelConfig())
    assertEquals(
      spec(ProviderConfig(Some("openai"), key = Some("${DEEPSEEK_API_KEY}"), models = models)).apiKey,
      Some("sk-secret")
    )
    assertEquals(
      spec(ProviderConfig(Some("openai"), keyEnv = Some("DEEPSEEK_API_KEY"), models = models)).apiKey,
      Some("sk-secret")
    )
    assertEquals(spec(ProviderConfig(Some("openai"), key = Some("literal"), models = models)).apiKey, Some("literal"))
    assertEquals(spec(ProviderConfig(Some("openai"), models = models)).apiKey, None)
    // and it never reaches a message or a log through toString
    val s = spec(ProviderConfig(Some("openai"), key = Some("${DEEPSEEK_API_KEY}"), models = models))
    assert(!s.toString.contains("sk-secret"), s.toString)

  test("the starting keys file binds the variables the starting config names"):
    val named = "\\$\\{([A-Za-z_][A-Za-z0-9_]*)\\}".r
      .findAllMatchIn(Config.globalTemplate).map(_.group(1).nn).toSet - "ATC_SESSION" // filled in per conversation
    val bound = Config.keysTemplate.linesIterator.filter(_.contains("=")).map(_.takeWhile(_ != '=').trim).toSet
    assertEquals(named -- bound, Set.empty[String], "every ${VAR} the config names should have a line to fill in")

  // ── Configuration.fileRules ──────────────────────────────────────────────

  test("fileRules carries each rule's origin and parses its access level"):
    val dir = Files.createTempDirectory("atc-rules").nn
    val cfg = writeCfg(
      dir,
      "config.json",
      """
      { "files": [ { "path": ".", "access": "read" },
                   { "path": "build", "access": "write", "locked": true },
                   { "path": "secrets", "classified": true } ] }
    """
    )
    val rules = load(dir, Some(cfg)).fileRules(dir)
    val configured = rules
    assertEquals(configured.map(_.access), List(Some(Access.Read), Some(Access.Write), None))
    assert(configured.forall(_.grantsWithin.isEmpty), "an explicit -c file grants wherever it matches")
    assert(configured(1).locked)
    assert(configured(2).pattern.matches(dir.resolve("sub/secrets/k")))
    // the summary folds the classified-only rules into one line
    val summary = Policy(rules, Nil, Nil, _ => Decision.Deny).summary
    assertEquals(summary.linesIterator.count(_.contains("classified patterns")), 1, summary)
    assert(summary.contains("classified patterns: secrets"), summary)

  test("contextWindow takes a number, a numeric string, or k/m suffixes; anything else is a clear error"):
    def parsed(json: String): Option[Int] =
      upickle.default.read[ModelConfig](ujson.read(json)).contextWindow.map(_.toInt)
    assertEquals(parsed("""{ "contextWindow": 200000 }"""), Some(200000))
    assertEquals(parsed("""{ "contextWindow": "200000" }"""), Some(200000))
    assertEquals(parsed("""{ "contextWindow": "256k" }"""), Some(256000))
    assertEquals(parsed("""{ "contextWindow": "1m" }"""), Some(1000000))
    assertEquals(parsed("""{ "contextWindow": "1M" }"""), Some(1000000))
    assertEquals(parsed("""{ "contextWindow": "1.5m" }"""), Some(1500000))
    assertEquals(parsed("""{ "contextWindow": " 128 K " }"""), Some(128000))
    assertEquals(parsed("""{}"""), None)
    for bad <- List("\"abc\"", "\"-5k\"", "\"\"", "0", "-1", "\"0k\"", "1.5", "3e9", "true", "\"1g\"") do
      intercept[Exception](parsed(s"""{ "contextWindow": $bad }"""))
    // every Tokens is a positive count, however it is made
    intercept[IllegalArgumentException](Tokens(0))
    intercept[IllegalArgumentException](Tokens(-5))
    assertEquals(Tokens(1).toInt, 1)
    assertEquals(Tokens.from(200000L).map(_.toInt), Some(200000))
    assertEquals(Tokens.from(0L), None)
    assertEquals(Tokens.from(Int.MaxValue.toLong + 1), None)
    // Tokens.parse is the same reader, for anything that wants to accept the notation
    assertEquals(Tokens.parse("64k").toInt, 64000)
    intercept[IllegalArgumentException](Tokens.parse("lots"))
    // and it survives a full config load
    val dir = Files.createTempDirectory("atc-cfg-ctx").nn
    val cfg = writeCfg(
      dir,
      "c.json",
      """{ "providers": { "p": { "api": "echo", "models": { "m": { "contextWindow": "32k" } } } } }"""
    )
    assertEquals(load(dir, Some(cfg)).settings.providers("p").models("m").contextWindow.map(_.toInt), Some(32000))

  test("predictInput is on by default and a later layer can turn it off"):
    assertEquals(upickle.default.read[Config](ujson.read("{}")).predictInput, true)
    val merged =
      Configuration.mergeJson(
        ujson.read("""{ "predictInput": true }""").obj,
        ujson.read("""{ "predictInput": false }""").obj
      )
    assertEquals(upickle.default.read[Config](merged).predictInput, false)

  // ── template ────────────────────────────────────────────────────

  test("the --init-global template is valid JSON that parses into a Config"):
    val json = Config.globalTemplate
    val c = upickle.default.read[Config](ujson.read(json))
    assert(c.providers.nonEmpty, "template should define providers")
    assert(c.files.nonEmpty, "template should define file rules")

  test("the settings the starting config shows are the code defaults"):
    val start = ConfigLayer.bundled.config
    assertEquals(start.mode, Some(Mode.Full.label), "the mode an unset one stands for")
    val shown = start.copy(providers = Map.empty, files = Nil, denyCommands = Nil, denyHosts = Nil, mode = None)
    assertEquals(shown, Config())

  // ── editing a config in place (`/model` remembers the choice) ───

  test("withTopLevel replaces an existing key's value and leaves the rest of the text alone"):
    val text =
      "{\n  \"model\": \"chat\",\n  \"classifiedModel\": \"local\",\n\n  \"commands\": [\"git status\", \"git log\"]\n}\n"
    val out = ObjectText.withTopLevel(text, "model", ujson.Str("anthropic/sonnet"))
    assertEquals(out, text.replace("\"model\": \"chat\"", "\"model\": \"anthropic/sonnet\""))
    // a value that is not a string, e.g. unsetting the classified model
    val off = ObjectText.withTopLevel(text, "classifiedModel", ujson.Null)
    assertEquals(off, text.replace("\"classifiedModel\": \"local\"", "\"classifiedModel\": null"))
    assertEquals(ujson.read(off)("classifiedModel"), ujson.Null)

  test("withTopLevel adds a missing key first, or after the named key, matching the file's indentation"):
    val text = "{\n    \"files\": [ { \"path\": \".\" } ],\n    \"safeMode\": true\n}\n"
    val first = ObjectText.withTopLevel(text, "model", ujson.Str("gpt"))
    assertEquals(
      first,
      "{\n    \"model\": \"gpt\",\n    \"files\": [ { \"path\": \".\" } ],\n    \"safeMode\": true\n}\n"
    )
    val after = ObjectText.withTopLevel(first, "classifiedModel", ujson.Str("local"), after = List("model"))
    assertEquals(
      after,
      "{\n    \"model\": \"gpt\",\n    \"classifiedModel\": \"local\",\n    \"files\": [ { \"path\": \".\" } ],\n    \"safeMode\": true\n}\n"
    )
    // after a key that is the last member (no trailing comma to reuse)
    val last =
      ObjectText.withTopLevel("{\n  \"model\": \"a\"\n}", "classifiedModel", ujson.Str("b"), after = List("model"))
    assertEquals(last, "{\n  \"model\": \"a\",\n  \"classifiedModel\": \"b\"\n}")
    // one-line and empty objects
    assertEquals(
      ObjectText.withTopLevel("""{ "safeMode": true }""", "model", ujson.Str("x")),
      """{ "model": "x", "safeMode": true }"""
    )
    assertEquals(ObjectText.withTopLevel("{}", "model", ujson.Str("x")), "{\n  \"model\": \"x\"\n}")
    assertEquals(ObjectText.withTopLevel("{ }", "model", ujson.Str("x")), "{\n  \"model\": \"x\"\n}")

  test("withTopLevel preserves CRLF when it inserts a key"):
    val text = "{\r\n    \"files\": [],\r\n    \"safeMode\": true\r\n}\r\n"
    val after = ObjectText.withTopLevel(text, "model", ujson.Str("gpt"), after = List("files"))
    assertEquals(
      after,
      "{\r\n    \"files\": [],\r\n    \"model\": \"gpt\",\r\n    \"safeMode\": true\r\n}\r\n",
    )
    val empty = ObjectText.withTopLevel("{\r\n}\r\n", "model", ujson.Str("gpt"))
    assertEquals(empty, "{\r\n  \"model\": \"gpt\"\r\n}\r\n")

  test("withTopLevel preserves bare CR and a BOM when it inserts a key"):
    val text = "\uFEFF{\r    \"files\": [],\r    \"safeMode\": true\r}\r"
    val after = ObjectText.withTopLevel(text, "model", ujson.Str("gpt"), after = List("files"))
    assertEquals(
      after,
      "\uFEFF{\r    \"files\": [],\r    \"model\": \"gpt\",\r    \"safeMode\": true\r}\r",
    )
    assertEquals(
      ObjectText.withTopLevel("\uFEFF{\r}\r", "model", ujson.Str("gpt")),
      "\uFEFF{\r  \"model\": \"gpt\"\r}\r"
    )

  test("withTopLevel only touches the top level: nested keys, strings and brackets do not confuse it"):
    val text =
      """{
        |  "providers": { "p": { "api": "openai", "models": { "model": { "name": "m }, \" { [" } } } },
        |  "hosts": ["a.example", "b}example"],
        |  "instructions": "no \"model\" here, {and} [there]",
        |  "model": "old"
        |}""".stripMargin
    val out = ObjectText.withTopLevel(text, "model", ujson.Str("new"))
    assertEquals(out, text.replace("\"model\": \"old\"", "\"model\": \"new\""))
    assertEquals(ujson.read(out)("providers")("p")("models")("model")("name").str, "m }, \" { [")
    // the whole template survives a round trip with only the value changed
    val template = Config.projectTemplate
    val edited = ObjectText.withTopLevel(template, "safeMode", ujson.False)
    assertEquals(edited, template.replace("\"safeMode\": true", "\"safeMode\": false"))
    assertEquals(upickle.default.read[Config](ujson.read(edited)).safeMode, false)

  test("withTopLevel rejects text that is not a JSON object"):
    intercept[IllegalArgumentException](ObjectText.withTopLevel("[1, 2]", "model", ujson.Str("x")))
    intercept[IllegalArgumentException](ObjectText.withTopLevel("{ oops", "model", ujson.Str("x")))

  test("withTopLevel edits the LAST of duplicated keys (the one the JSON reader honors)"):
    val text = "{\n  \"model\": \"a\",\n  \"model\": \"b\"\n}\n"
    val out = ObjectText.withTopLevel(text, "model", ujson.Str("c"))
    assertEquals(out, "{\n  \"model\": \"a\",\n  \"model\": \"c\"\n}\n")
    assertEquals(ujson.read(out)("model").str, "c") // the effective value actually changed

  // ── project detection, key files, init ──────────────────────────

  test("a .atc holding only keys.properties still marks a project root"):
    val dir = Files.createTempDirectory("atc-projkeys").nn.toRealPath().nn
    val sub = Files.createDirectories(dir.resolve("sub")).nn
    Files.createDirectories(dir.resolve(".atc"))
    Files.writeString(dir.resolve(".atc").resolve(Config.KeysFile), "K=v\n")
    assertEquals(Config.projectRoot(sub), Some(dir))

  test("KeyBindings fall through to the environment after the files"):
    val keys = KeyBindings.load(Nil)
    assertEquals(keys.get("PATH"), Option(System.getenv("PATH")).filter(_.nonEmpty))

  test("ensureGlobal creates the keys file readable by the owner only"):
    val dir = Files.createTempDirectory("atc-ensure").nn
    val created = Config.ensureGlobal(dir.resolve("config.json").nn)
    assertEquals(created.size, 2)
    val keys = dir.resolve(Config.KeysFile).nn
    val posix = Files.getFileAttributeView(keys, classOf[java.nio.file.attribute.PosixFileAttributeView])
    if posix != null then
      import java.nio.file.attribute.PosixFilePermission as P
      val perms = Files.getPosixFilePermissions(keys).nn
      assert(!perms.contains(P.GROUP_READ) && !perms.contains(P.OTHERS_READ), perms.toString)
    else assert(Files.isRegularFile(keys), keys.toString)

  test("initProject adds keys.properties to a pre-existing .atc/.gitignore"):
    val dir = Files.createTempDirectory("atc-initproj").nn
    Files.createDirectories(dir.resolve(".atc"))
    Files.writeString(dir.resolve(".atc/.gitignore"), "target\n")
    val created = Config.initProject(dir, dir.resolve("no-such-global.json").nn)
    assert(created.contains(Config.projectPath(dir)), created.toString)
    val gi = Files.readString(dir.resolve(".atc/.gitignore")).nn
    assert(gi.startsWith("target\n") && gi.linesIterator.exists(_.trim == Config.KeysFile), gi)
    // already covered: untouched
    val dir2 = Files.createTempDirectory("atc-initproj2").nn
    Files.createDirectories(dir2.resolve(".atc"))
    Files.writeString(dir2.resolve(".atc/.gitignore"), "keys.properties\n")
    Config.initProject(dir2, dir2.resolve("no-such-global.json").nn)
    assertEquals(Files.readString(dir2.resolve(".atc/.gitignore")).nn, "keys.properties\n")
    val dir3 = Files.createTempDirectory("atc-initproj3").nn
    Files.createDirectories(dir3.resolve(".atc"))
    Files.writeString(dir3.resolve(".atc/.gitignore"), "keys.properties\n!keys.properties\n")
    Config.initProject(dir3, dir3.resolve("no-such-global.json").nn)
    assertEquals(
      Files.readString(dir3.resolve(".atc/.gitignore")).nn,
      "keys.properties\n!keys.properties\nkeys.properties\n"
    )
    val dir4 = Files.createTempDirectory("atc-initproj4").nn
    Files.createDirectories(dir4.resolve(".atc"))
    Files.writeString(dir4.resolve(".atc/.gitignore"), "target\r\n")
    Config.initProject(dir4, dir4.resolve("no-such-global.json").nn)
    assertEquals(Files.readString(dir4.resolve(".atc/.gitignore")).nn, "target\r\nkeys.properties\r\n")
    val dir5 = Files.createTempDirectory("atc-initproj5").nn
    Files.createDirectories(dir5.resolve(".atc"))
    Files.writeString(dir5.resolve(".atc/.gitignore"), "")
    Config.initProject(dir5, dir5.resolve("no-such-global.json").nn)
    assertEquals(Files.readString(dir5.resolve(".atc/.gitignore")).nn, "keys.properties\n")
    val dir6 = Files.createTempDirectory("atc-initproj6").nn
    Files.createDirectories(dir6.resolve(".atc"))
    Files.writeString(dir6.resolve(".atc/.gitignore"), "\uFEFFtarget\r")
    Config.initProject(dir6, dir6.resolve("no-such-global.json").nn)
    assertEquals(Files.readString(dir6.resolve(".atc/.gitignore")).nn, "\uFEFFtarget\rkeys.properties\r")

  test("an invalid reasoning effort or summary is a config error, not a per-turn crash"):
    def withModel(model: String): IllegalArgumentException =
      val dir = Files.createTempDirectory("atc-cfg-reasoning").nn
      val cfg =
        writeCfg(dir, "config.json", s"""{ "providers": { "p": { "api": "echo", "models": { "m": $model } } } }""")
      intercept[IllegalArgumentException](load(dir, Some(cfg)))
    assert(withModel("""{ "reasoning": "hihg" }""").getMessage.nn.contains("reasoning"))
    assert(withModel("""{ "reasoningSummary": "loud" }""").getMessage.nn.contains("reasoningSummary"))
    // valid values pass
    val dir = Files.createTempDirectory("atc-cfg-reasoning-ok").nn
    val cfg = writeCfg(
      dir,
      "config.json",
      """{ "providers": { "p": { "api": "echo", "models": { "m": { "reasoning": "high", "reasoningSummary": "auto" } } } } }"""
    )
    assertEquals(load(dir, Some(cfg)).settings.providers("p").models("m").reasoning, Some("high"))

  test("provider APIs, web-search versions, and case-insensitive model references validate at load time"):
    def rejected(providers: String): IllegalArgumentException =
      val dir = Files.createTempDirectory("atc-cfg-provider-validation").nn
      val cfg = writeCfg(dir, "config.json", s"""{ "providers": $providers }""")
      intercept[IllegalArgumentException](load(dir, Some(cfg)))
    assert(rejected("""{ "p": { "api": "opneai", "models": {} } }""").getMessage.nn.contains("api"))
    assert(rejected(
      """{ "p": { "api": "anthropic", "models": { "m": { "webSearchVersion": "2026509" } } } }"""
    ).getMessage.nn.contains("webSearchVersion"))
    assert(rejected(
      """{ "p": { "api": "anthropic", "models": { "m": { "reasoning": " high " } } } }"""
    ).getMessage.nn.contains("whitespace"))
    assert(rejected(
      """{ "p": { "api": "anthropic", "models": { "m": { "webSearchVersion": " 20250305 " } } } }"""
    ).getMessage.nn.contains("whitespace"))
    assert(rejected(
      """{ "p": { "api": "echo", "models": { "m": { "displayName": "   " } } } }"""
    ).getMessage.nn.contains("displayName"))
    assert(rejected(
      """{ "p": { "api": "echo", "models": { "m": { "displayName": " Friendly " } } } }"""
    ).getMessage.nn.contains("whitespace"))
    assert(rejected(
      """{ "p": { "api": "echo", "models": { "m": { "displayName": "First\nSecond" } } } }"""
    ).getMessage.nn.contains("single line"))
    val duplicate = rejected(
      """{ "P": { "api": "echo", "models": { "M": {} } },
        |    "p": { "api": "echo", "models": { "m": {} } } }""".stripMargin
    )
    assert(duplicate.getMessage.nn.contains("unique ignoring case"), duplicate.getMessage)
    assert(rejected("""{ " p": { "api": "echo", "models": { "m": {} } } }""")
      .getMessage.nn.contains("provider name"))
    assert(rejected("""{ "p": { "api": "echo", "models": { "m ": {} } } }""")
      .getMessage.nn.contains("model alias"))
    // A control character in a reference would reach the terminal wherever the model is named.
    val alias = rejected("""{ "p": { "api": "echo", "models": { "m\u001b[2J": {} } } }""").getMessage.nn
    assert(alias.contains("""model alias 'm\u001b[2J' must not contain control characters"""), alias)
    val provider = rejected("""{ "p\u0007q": { "api": "echo", "models": { "m": {} } } }""").getMessage.nn
    assert(provider.contains("""provider name 'p\u0007q' must not contain control characters"""), provider)

  test("setTopLevel rewrites the project config, and a null classifiedModel there unsets a global one"):
    val dir = Files.createTempDirectory("atc-cfg-set").nn
    val projectDir = Files.createDirectories(dir.resolve(".atc")).nn
    val project = writeCfg(projectDir, "config.json", "{\n  \"model\": \"a\",\n  \"commands\": [\"ls\"]\n}\n")
    val global = writeCfg(
      dir,
      "global.json",
      """{ "model": "b", "classifiedModel": "b",
        |  "providers": { "p": { "api": "echo", "models": { "a": {}, "b": {} } } } }""".stripMargin
    )
    val posix = Files.getFileAttributeView(project, classOf[java.nio.file.attribute.PosixFileAttributeView])
    val permissions = Option(posix).map(_ => Files.getPosixFilePermissions(project).nn)
    Config.setTopLevel(project, "model", ujson.Str("b"))
    Config.setTopLevel(project, "classifiedModel", ujson.Null, after = List("model"))
    assertEquals(
      Files.readString(project),
      "{\n  \"model\": \"b\",\n  \"classifiedModel\": null,\n  \"commands\": [\"ls\"]\n}\n"
    )
    permissions.foreach(p => assertEquals(Files.getPosixFilePermissions(project).nn, p))
    val loaded = Config.load(dir, None, global, trustProject = true)
    assertEquals(loaded.settings.model, Some("b"))
    assertEquals(loaded.settings.classifiedModel, None)
    assertEquals(loaded.settings.commands, List("ls"))

  test("setTopLevel preserves an intentional shared-config symlink"):
    val dir = Files.createTempDirectory("atc-cfg-set-link").nn
    val victim = writeCfg(dir, "victim.json", "{ \"model\": \"a\" }")
    val link = dir.resolve("link.json").nn
    assume(TestEnv.trySymbolicLink(link, victim), "symbolic links are unavailable for this account")
    Config.setTopLevel(link, "model", ujson.Str("b"))
    assert(Files.isSymbolicLink(link), "an intentional shared-config symlink must survive")
    assertEquals(ujson.read(Files.readString(victim))("model").str, "b")

  test("autoCompactThreshold defaults to 80 percent and validates fractions including off"):
    assertEquals(upickle.default.read[Config]("{}").autoCompactThreshold, 0.8)
    List(0.0, 0.7, 1.0).foreach: value =>
      val parsed = upickle.default.read[Config](s"""{"autoCompactThreshold": $value}""")
      assertEquals(ConfigValidation.validate(parsed).autoCompactThreshold, value)
    List(-0.1, 1.01, Double.NaN, Double.PositiveInfinity).foreach: value =>
      val error = intercept[IllegalArgumentException](ConfigValidation.validate(Config(autoCompactThreshold = value)))
      assert(error.getMessage.nn.contains("autoCompactThreshold"))

  test("autoCompactThreshold follows normal global, project and explicit precedence"):
    def layer(origin: Origin, json: String): ConfigLayer =
      val obj = ujson.read(json).obj
      ConfigLayer(origin, None, obj, upickle.default.read[Config](obj), None)
    val global = layer(Origin.Global, """{"autoCompactThreshold": 0.9}""")
    val project = layer(Origin.Project, """{"autoCompactThreshold": 0.6}""")
    val explicit = layer(Origin.Explicit, """{"autoCompactThreshold": 0}""")
    assertEquals(Configuration.combine(List(global, project)).settings.autoCompactThreshold, 0.6)
    assertEquals(Configuration.combine(List(global, project, explicit)).settings.autoCompactThreshold, 0.0)

  test("effort accepts a known effort or default"):
    List("high", "none", "default").foreach: value =>
      assertEquals(ConfigValidation.validate(Config(effort = Some(value))).effort, Some(value))
    val error = intercept[IllegalArgumentException](ConfigValidation.validate(Config(effort = Some("turbo"))))
    assert(error.getMessage.nn.contains("effort"), error.getMessage)

  test("editFile leaves a file alone when the edit changes nothing"):
    val file = Files.createTempFile("atc-edit", ".json").nn
    Files.writeString(file, """{ "model": "a" }""")
    Files.setLastModifiedTime(file, java.nio.file.attribute.FileTime.fromMillis(0))
    Config.editFile(file)(ObjectText.withMember(_, List("effort"), None))
    assertEquals(Files.getLastModifiedTime(file).toMillis, 0L)
    Config.editFile(file)(ObjectText.withTopLevel(_, "effort", ujson.Str("high"), after = List("model")))
    assertEquals(Files.readString(file), """{ "model": "a", "effort": "high" }""")
    Config.editFile(file)(ObjectText.withMember(_, List("effort"), None))
    assertEquals(Files.readString(file), """{ "model": "a" }""")

  test("a config or key write with a lone surrogate fails and leaves the file unchanged"):
    val dir = Files.createTempDirectory("atc-strict-utf8").nn
    val config = writeCfg(dir, "config.json", """{ "model": "a" }""")
    val keys = dir.resolve("keys.properties").nn
    try
      intercept[CharacterCodingException](Config.editFile(config)(_ + "\uD800"))
      assertEquals(Files.readString(config), """{ "model": "a" }""")
      intercept[CharacterCodingException](KeyBindings.bind(keys, "A_KEY", "\uD800"))
      assert(!Files.exists(keys))
      KeyBindings.bind(keys, "A_KEY", "one")
      val bound = Files.readString(keys)
      intercept[CharacterCodingException](KeyBindings.bind(keys, "A_KEY", "two\uD800"))
      assertEquals(Files.readString(keys), bound)
      val entries = Files.list(dir).nn
      try assertEquals(entries.count(), 2L)
      finally entries.close()
    finally List(config, keys, dir).foreach(Files.deleteIfExists)

  test("notifications accepts the known methods only"):
    assertEquals(upickle.default.read[Config]("{}").notifications, "auto")
    List("auto", "system", "terminal", "bell", "off", "Bell").foreach: value =>
      assertEquals(ConfigValidation.validate(Config(notifications = value)).notifications, value)
    val error = intercept[IllegalArgumentException](ConfigValidation.validate(Config(notifications = "popup")))
    assert(error.getMessage.nn.contains("notifications"), error.getMessage)

  test("compactKeepRatio validates fractions and follows layer precedence"):
    assertEquals(upickle.default.read[Config]("{}").compactKeepRatio, 0.2)
    List(0.0, 0.3, 1.0).foreach: ratio =>
      val parsed = upickle.default.read[Config](s"""{"compactKeepRatio": $ratio}""")
      assertEquals(ConfigValidation.validate(parsed).compactKeepRatio, ratio)
    List(-0.1, 1.01, Double.NaN, Double.PositiveInfinity).foreach: ratio =>
      intercept[IllegalArgumentException](ConfigValidation.validate(Config(compactKeepRatio = ratio)))
    def layer(origin: Origin, ratio: Double): ConfigLayer =
      val json = ujson.Obj("compactKeepRatio" -> ratio)
      ConfigLayer(origin, None, json, upickle.default.read[Config](json), None)
    val global = layer(Origin.Global, 0.3)
    val project = layer(Origin.Project, 0.4)
    val explicit = layer(Origin.Explicit, 0.1)
    assertEquals(Configuration.combine(List(global, project)).settings.compactKeepRatio, 0.4)
    assertEquals(Configuration.combine(List(global, project, explicit)).settings.compactKeepRatio, 0.1)

  test("an empty literal key is not a binding"):
    assertEquals(KeyBindings.empty.resolve(""), None)
    assertEquals(KeyBindings.empty.resolve("sk-1"), Some("sk-1"))
    val p = ProviderConfig(api = Some("anthropic"), key = Some(""), keyEnv = Some("K"))
    assertEquals(
      KeyBindings(List((java.nio.file.Path.of("k"), Map("K" -> "from-env")))).apiKey(p),
      Some("from-env")
    )

  test("provider headers resolve ${VAR} values, drop unset ones and keep the session placeholder"):
    val p = ProviderConfig(
      api = Some("openai"),
      headers = Map(
        "x-literal" -> "plain",
        "x-secret" -> "${HDR}",
        "x-unset" -> "${NO_SUCH_HEADER_VAR_XYZ}",
        "x-session" -> ProviderConfig.SessionRef,
      ),
    )
    val bindings = KeyBindings(List((java.nio.file.Path.of("k"), Map("HDR" -> "from-file"))))
    assertEquals(
      bindings.headers(p),
      Map("x-literal" -> "plain", "x-secret" -> "from-file", "x-session" -> "${ATC_SESSION}"),
    )
    assertEquals(KeyBindings.empty.headers(ProviderConfig(api = Some("openai"))), Map.empty)

  // ── efforts and listed models ───────────────────────────────────

  private def withModel(model: String): Config =
    upickle.default.read[Config](
      s"""{ "providers": { "p": { "api": "anthropic", "models": { "m": $model } } } }"""
    )

  test("efforts must be known efforts and include the starting one"):
    ConfigValidation.validate(withModel("""{ "reasoning": "high", "efforts": ["low", "high"] }"""))
    val unknown =
      intercept[IllegalArgumentException](ConfigValidation.validate(withModel("""{ "efforts": ["low", "huge"] }""")))
    assert(unknown.getMessage.contains("providers.p.models.m.efforts"), unknown.getMessage)
    val outside =
      intercept[IllegalArgumentException](
        ConfigValidation.validate(withModel("""{ "reasoning": "max", "efforts": ["low"] }"""))
      )
    assert(outside.getMessage.contains("not one of its efforts"), outside.getMessage)

  private def listing = upickle.default.read[Config](
    """{ "providers": {
      "fixed": { "api": "openai", "models": { "a": { "name": "model-a" } } },
      "open":  { "api": "openai", "url": "http://localhost:1" },
      "keyed": { "api": "openai", "key": "${ATC_TEST_UNSET_KEY}" },
      "echo":  { "api": "echo" }
    } }"""
  )

  private def listingCatalog(list: ModelSpec => List[ModelSpec]): (ModelCatalog, () => List[String]) =
    val asked = java.util.concurrent.ConcurrentLinkedQueue[String]()
    val catalog = ModelCatalog.from(
      listing,
      discover = Some: p =>
        asked.add(p.provider)
        list(p)
    )
    (catalog, () => scala.jdk.CollectionConverters.IterableHasAsScala(asked).asScala.toList)

  private def listed(p: ModelSpec, ids: String*) = ids.toList.map(id => p.copy(alias = id, modelId = id))

  test("a provider without models is listed once, when needed, and its models are named by reference"):
    val (catalog, asked) = listingCatalog(p => listed(p, "vendor/big", "small"))
    assertEquals(catalog.find("a").modelId, "model-a")
    assertEquals(asked(), Nil, "a configured model needs no listing")
    assertEquals(
      catalog.discoverable.map(_.provider),
      List("open"),
      "echo and a provider with an unset key are not asked"
    )
    assertEquals(catalog.models.map(catalog.label), List("a", "open/vendor/big", "open/small"))
    assertEquals(catalog.find("open/vendor/big").modelId, "vendor/big")
    assertEquals(catalog.find("SMALL").ref, "open/small")
    assertEquals(catalog.find("open/small").baseUrl, Some("http://localhost:1"))
    val missing = intercept[IllegalArgumentException](catalog.find("open/typo"))
    assert(missing.getMessage.contains("open list their own"), missing.getMessage)
    assertEquals(asked(), List("open"))

  test("a lookup never waits for a list, and a provider named in a reference keeps it to that provider"):
    val withOff =
      listing.copy(providers = listing.providers + ("off" -> ProviderConfig(Some("openai"), enabled = false)))
    val asked = java.util.concurrent.ConcurrentLinkedQueue[String]()
    val catalog = ModelCatalog.from(
      withOff,
      discover = Some: p =>
        asked.add(p.provider)
        listed(p, "small", "fixed/b", "off/c", "keyed/d")
    )
    // a bare id that no list has named yet is unknown, without fetching a list for it
    intercept[IllegalArgumentException](catalog.find("small"))
    assert(asked.isEmpty, asked.toString)
    assertEquals(catalog.labels, List("a"), "completion offers what is known without fetching a list")
    assert(asked.isEmpty, asked.toString)
    catalog.models // the lists arrive
    assertEquals(catalog.find("small").ref, "open/small")
    assertEquals(catalog.find("open/fixed/b").ref, "open/fixed/b")
    // these name models of the providers `fixed`, `off` and `keyed`, which have none of that name
    for reference <- List("fixed/b", "off/c", "keyed/d") do
      val e = intercept[IllegalArgumentException](catalog.find(reference))
      assert(e.getMessage.contains("Unknown model"), e.getMessage)

  test("a failed listing is ignored and a provider/model-id reference is taken as given"):
    val (catalog, _) = listingCatalog(_ => throw RuntimeException("connection refused"))
    assertEquals(catalog.find("open/anything").modelId, "anything")
    assertEquals(catalog.models.map(catalog.label), List("a"))
    intercept[IllegalArgumentException](catalog.find("anything"))

  test("validation leaves model names to the start, which warns about one that names no model"):
    ConfigValidation.validate(listing.copy(model = Some("open/gpt-x"), classifiedModel = Some("gpt-y")))
    val noListing = listing.copy(providers = listing.providers - "open")
    ConfigValidation.validate(noListing.copy(model = Some("gpt-y"), classifiedModel = Some("typo")))

  test("the top-level webSearch applies to every model that does not set its own"):
    val c = upickle.default.read[Config](
      """{ "webSearch": true, "providers": {
        "p": { "api": "openai", "models": { "on": {}, "off": { "webSearch": false } } },
        "q": { "api": "openai", "url": "http://localhost:1" } } }"""
    )
    val catalog = ModelCatalog.from(c, discover = Some(p => List(p.copy(alias = "listed", modelId = "listed"))))
    assertEquals(catalog.find("on").settings.webSearch, Some(true))
    assertEquals(catalog.find("off").settings.webSearch, Some(false))
    assertEquals(catalog.find("q/listed").settings.webSearch, Some(true))
    assertEquals(ModelCatalog.from(c.copy(webSearch = None)).find("on").settings.webSearch, None)

  test("a stored list names models without fetching, and a refresh replaces it in the background"):
    val store = ModelListStore(Files.createTempDirectory("atc-lists").nn.resolve("model-lists.json").nn)
    val open = ModelCatalog.from(listing).discoverable.head
    store.save(
      open,
      List(open.copy(alias = "old", modelId = "old", settings = ModelConfig(efforts = Some(List("high")))))
    )
    val release = java.util.concurrent.CountDownLatch(1)
    val catalog = ModelCatalog.from(
      listing.copy(webSearch = Some(true)),
      discover = Some { p =>
        release.await()
        listed(p, "new")
      },
      store = Some(store),
    )
    val old = catalog.find("open/old")
    assertEquals(old.settings.efforts, Some(List("high")), "found without waiting for a fetch")
    assertEquals(old.settings.webSearch, Some(true), "the current defaults apply to a stored list")
    assertEquals(catalog.find("open/unlisted").modelId, "unlisted", "an unknown id is taken as given before a fetch")
    catalog.refresh() // returns at once, the fetch is still blocked
    assertEquals(catalog.find("old").ref, "open/old")
    release.countDown()
    assertEquals(catalog.models.map(catalog.label), List("a", "open/new"))
    intercept[IllegalArgumentException](catalog.find("open/old"))
    assertEquals(store.load(open).map(_.map(_.modelId)), Some(List("new")), "the fetched list is stored")
    assertEquals(store.load(open.copy(baseUrl = Some("http://elsewhere"))), None, "only for the same endpoint")
