package atc

import atc.perms.Decision
import atc.sandbox.*

/** The sandbox REPL executor: language coverage, error reporting, REPL
  * command allow-list, validator integration, timeouts, output capture,
  * safe-mode on/off and session isolation (migrated from TACIT's
  * ScalaExecutorSuite / SessionManagerSuite / CodeValidatorEvasionSuite e2e). */
class ReplSessionSuite extends munit.FunSuite, ReplAssertions:
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  // Each session gets its own host so that per-session routing can be checked.
  val envSafe = TestEnv(prefix = "atc-repl-safe")
  val envNoSafe = TestEnv(prefix = "atc-repl-nosafe")
  val envQuick = TestEnv(prefix = "atc-repl-quick")
  lazy val safe: ReplSession = envSafe.newSession()
  lazy val nosafe: ReplSession = envNoSafe.newSession(safeMode = false)
  lazy val quick: ReplSession = envQuick.newSession(timeoutMs = Some(1000L))

  private def run(code: String): ExecutionResult = safe.run(code)

  /** Run `code` on another thread, `stop` the session once the evaluation is under way,
    * and return the result; the evaluation must end within ten seconds. */
  private def stopped(session: ReplSession, code: String)(stop: ReplSession => Unit)(using
    munit.Location
  ): ExecutionResult =
    @volatile var result: Option[ExecutionResult] = None
    val thread = Thread(() => result = Some(session.run(code)))
    thread.setDaemon(true)
    thread.start()
    Thread.sleep(700)
    stop(session)
    thread.join(10000)
    assert(!thread.isAlive, "the evaluation did not stop")
    result.getOrElse(fail("the evaluation gave no result"))

  // ── Basic execution ─────────────────────────────────────────────

  test("simple expression") { assert(assertOk(run("1 + 1")).output.contains("2")) }
  test("println goes through the host and into the tool output"):
    envSafe.clearOutput()
    val r = assertOk(run("""println("Hello, World!")"""))
    assert(r.output.contains("Hello, World!"), r.output)
    assertEquals(envSafe.agentOut.toString, "Hello, World!\n")
    assertEquals(envSafe.userOut.toString, "Hello, World!\n")
  test("val definition is echoed"):
    val r = assertOk(run("val fortyTwo = 42"))
    assert(r.output.contains("val fortyTwo: Int = 42"), r.output)
  test("echoes are the compiler's rendering, values unchanged"):
    val r = assertOk(run("val pr = ProcessResult(0, \"\", \"\"); val marker = \"^{}\""))
    assert(r.output.contains("stdout = 0 chars"), r.output)
    assert(r.output.contains("val marker: String = \"^{}\""), r.output)
  test("function definition and call"):
    assert(assertOk(run("def add(a: Int, b: Int): Int = a + b\nadd(2, 3)")).output.contains("5"))
  test("List map") { assert(assertOk(run("List(1, 2, 3).map(_ * 2)")).output.contains("List(2, 4, 6)")) }
  test("foreach println on a List of String"):
    val r = assertOk(run("""List("hello", "world").foreach(println)"""))
    assert(r.output.contains("hello") && r.output.contains("world"), r.output)
  test("Map") {
    assert(assertOk(run("""Map("a" -> 1, "b" -> 2).values.toList.sorted""")).output.contains("List(1, 2)"))
  }
  test("java.time is available"):
    assert(assertOk(run("import java.time.LocalDate\nLocalDate.of(2025, 1, 1).getYear")).output.contains("2025"))
  test("scala.util.Try is available"):
    assert(assertOk(run("import scala.util.Try\nTry(\"123\".toInt).isSuccess")).output.contains("true"))
  test("case class with method"):
    val r = assertOk(run("""
      case class Point(x: Int, y: Int):
        def distTo(other: Point): Double =
          math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
      Point(0, 0).distTo(Point(3, 4))
    """))
    assert(r.output.contains("5.0"), r.output)
  test("pattern matching"):
    val r = assertOk(run("val any: Any = 42\nany match\n  case i: Int => s\"int: $i\"\n  case _ => \"other\""))
    assert(r.output.contains("int: 42"), r.output)
  test("higher-order functions"):
    assert(assertOk(run("List(1, 2, 3, 4, 5).filter(_ % 2 == 0).map(_ * 10)")).output.contains("List(20, 40)"))
  test("for comprehension"):
    assert(assertOk(
      run("for\n  x <- List(1, 2, 3)\n  y <- List(10, 20)\nyield x * y")
    ).output.contains("List(10, 20, 20, 40, 30, 60)"))
  test("string interpolation and chaining"):
    val r = assertOk(run("val n = 3; val s = s\"n=${n * 2}\"; List(1,2,3)\n  .map(_ * 2)\n  .filter(_ > 2)\n  .sum"))
    assert(r.output.contains("n=6") && r.output.contains("10"), r.output)
  test("object, enum and lazy val definitions"):
    assertOk(run("object Counter { def x = 1 }\nenum Color { case Red, Green }\nlazy val lz = Color.Red"))
    assert(assertOk(run("Counter.x + 41")).output.contains("42"))
    assert(assertOk(run("lz.toString")).output.contains("Red"))
  test("large output completes"):
    val r = assertOk(run("(1 to 500).toList"))
    assert(r.output.replace(" ", "").replace("\n", "").contains("List(1,2,3"))
  test("a warning does not fail the evaluation"):
    val r = assertOk(run("Some(1) match { case Some(v) => v; case None => 0 }"))
    assert(r.output.toLowerCase.contains("warning"), r.output)

  // ── State ───────────────────────────────────────────────────────

  test("state persists across runs"):
    assertOk(run("val persisted = 41"))
    assert(assertOk(run("persisted + 1")).output.contains("42"))
  test("imports persist across runs"):
    assertOk(run("import java.time.Duration"))
    assert(assertOk(run("Duration.ofSeconds(90).toMinutes")).output.contains("1"))
  test("a failed run leaves the previous state intact"):
    assertOk(run("val keep = \"kept\""))
    assertFails(run("val keep2: Int = \"nope\""))
    assert(assertOk(run("keep")).output.contains("kept"))
  test("sessions are isolated from each other and keep their own host"):
    assertOk(safe.run("val shared = 1"))
    assertOk(nosafe.run("val shared = 2"))
    assert(assertOk(safe.run("shared")).output.contains("1"))
    assert(assertOk(nosafe.run("shared")).output.contains("2"))
    envSafe.clearOutput(); envNoSafe.clearOutput()
    assertOk(safe.run("""println("from-safe")"""))
    assertOk(nosafe.run("""println("from-nosafe")"""))
    assertEquals(envSafe.agentOut.toString, "from-safe\n")
    assertEquals(envNoSafe.agentOut.toString, "from-nosafe\n")

  test("a session selects its own host before lazily initializing preamble values"):
    // Initialize both sessions before either executes agent code. The process-
    // global Runtime slot now names B; A's first run must still obtain A's host
    // and capabilities rather than writing through B's file system.
    val a = TestEnv(prefix = "atc-lazy-host-a")
    val b = TestEnv(prefix = "atc-lazy-host-b")
    val sa = a.newSession()
    val sb = b.newSession()
    assertOk(sa.run("""write("owner.txt", "a")"""))
    assertOk(sb.run("""write("owner.txt", "b")"""))
    assertEquals(a.contents("owner.txt"), "a")
    assertEquals(b.contents("owner.txt"), "b")

  test("initializing a session does not replace the host of a session that is evaluating"):
    // The host slot is process-global, so only a holder of the evaluation lock may set it.
    val a = TestEnv(prefix = "atc-init-host-a")
    val b = TestEnv(prefix = "atc-init-host-b")
    val sa = a.newSession()
    val outside = TestEnv.outsideDir("init-host")
    @volatile var hostKept: Option[Boolean] = None
    val initB = Thread(() => { b.newSession(); () })
    initB.setDaemon(true)
    a.onRequest = _ =>
      initB.start()
      Thread.sleep(1000)
      hostKept = Some(atc.lib.Runtime.current eq a.host)
    a.decisions = List(Decision.AllowOnce)
    assertOk(sa.run(s"requestFiles(${a.scalaString(outside)}, Access.Read, \"x\") { 1 }"))
    initB.join(240000)
    assertEquals(hostKept, Some(true))

  // ── Errors ──────────────────────────────────────────────────────

  test("syntax error"):
    val r = assertFails(run("val x = def"), "syntax error")
    assert(r.error.exists(_.contains("Line 1")), r.error.toString)
  test("type error") { assertFails(run("""val x: Int = "hello""""), "type mismatch") }
  test("unknown identifier") { assertFails(run("nonexistentThing + 1"), "not found") }
  test("uncaught exception is reported as failure with the message"):
    val r = assertFails(run("""throw new RuntimeException("boom")"""), "boom")
    assert(r.output.contains("RuntimeException"))
  test("host frames are trimmed from traces but agent frames stay"):
    val r = assertFails(
      run("""def fail(): Int = throw new IllegalStateException("deep")
      |fail()""".stripMargin),
      "deep"
    )
    val rendered = r.render
    assert(!rendered.contains("at atc."), rendered)
    assert(!rendered.contains("at java."), rendered)
    assert(!rendered.contains("elided"), rendered)
    assert(rendered.contains("rs$line$"), rendered)
  test("printed lines that look like stack frames reach the model unchanged"):
    // A project's test runner prints traces too; only the trace the REPL renders is trimmed.
    val printed = "  at java.lang.Thread.run(Thread.java:1)\n  at scala.Option.map(Option.scala:1)\n  ... 3 more"
    val r = assertFails(
      run(
        """println("  at java.lang.Thread.run(Thread.java:1)\n  at scala.Option.map(Option.scala:1)\n  ... 3 more")
          |def failPrinted(): Int = throw new IllegalStateException("thrown")
          |failPrinted()""".stripMargin
      ),
      "thrown"
    )
    val rendered = r.render
    assert(rendered.contains(printed), rendered)
    assertEquals(rendered.split("at java\\.", -1).length - 1, 1, rendered) // the printed frame only
    assert(rendered.contains("rs$line$"), rendered)
  test("empty and whitespace-only code do not crash"):
    assertOk(run(""))
    assertOk(run("   \n\n  "))
  test("comment-only code is fine") { assertOk(run("// nothing to see\n/* here */")) }

  // ── Validator integration ───────────────────────────────────────

  test("validator rejects java.io before compilation"):
    val r = assertFails(run("import java.io.File\nval f = new File(\"/tmp\")\nf.isDirectory"), "file-io-java")
    assert(r.error.exists(_.contains("Code validation failed")))
    assertEquals(r.output, "")
  test("validator rejects scala.io.Source") { assertFails(run("import scala.io.Source"), "file-io-scala") }
  test("validator allows a forbidden token in a string literal"):
    assert(assertOk(run("""println("java.io is just a string")""")).output.contains("java.io is just a string"))
  test("validator: interpolation is not executed"):
    val r = assertFails(run("""val home = s"${System.getProperty("user.home")}"; home"""), "sys-getprop")
    assert(!r.output.contains(System.getProperty("user.home")))

  // ── REPL command allow-list ─────────────────────────────────────

  private def assertCommandRejected(cmd: String)(using munit.Location): Unit =
    val r = run(cmd)
    assert(!r.success, s"'$cmd' should be rejected")
    assert(r.error.exists(_.contains("Only :type, :doc, and :imports")), s"'$cmd' → ${r.error}")
  test("rejects :quit") { assertCommandRejected(":quit") }
  test("rejects :q") { assertCommandRejected(":q") }
  test("rejects :settings") { assertCommandRejected(":settings") }
  test("rejects :dep") { assertCommandRejected(":dep com.example::lib:1.0") }
  test("rejects :sh") { assertCommandRejected(":sh echo hello") }
  test("rejects :load") { assertCommandRejected(":load /tmp/evil.scala") }
  test("rejects :reset") { assertCommandRejected(":reset") }
  test("rejects :help") { assertCommandRejected(":help") }
  test("rejects :jar") { assertCommandRejected(":jar /tmp/x.jar") }
  test(":type works"):
    assertOk(run("val typed = 42"))
    assert(assertOk(run(":type typed")).output.contains("Int"))
  test(":imports shows the preamble and safe mode"):
    val out = assertOk(run(":imports")).output
    assert(out.contains("import atc.lib.*"), out)
    assert(out.contains("import api.*"), out)
    assert(out.contains("import language.experimental.safe"), out)
  test(":doc is allowed") { assertOk(run(":doc List")) }

  // ── Safe mode ───────────────────────────────────────────────────

  test("safe mode: top-level var is rejected") { assertFails(run("var counter = 0"), "mutable variable") }
  test("safe mode: harmless import aliases remain available"):
    val result = assertOk(run("import java.time.{Duration as JDuration}\nJDuration.ofSeconds(90).toMinutes"))
    assert(result.output.contains("1"), result.output)
    assertFails(run("import java.{io as jio}\nnew jio.File(\"/etc/passwd\").exists"), "safe code")
  test("safe mode: mutable collections are rejected"):
    assertFails(run("scala.collection.mutable.ArrayBuffer[String]()"), "safe code")
    assertFails(run("import scala.collection.mutable.ListBuffer\nListBuffer[Int]()"), "safe code")
  test("safe mode: the StringBuilder companion is rejected, but `new StringBuilder` works"):
    assertFails(run("val sb = StringBuilder()"), "safe code")
    val top = assertOk(run("val sb: StringBuilder = new StringBuilder()\nsb.append(\"x\")\nsb.toString"))
    assert(top.output.contains("x"), top.output)
    val local = assertOk(run("def b(): String = { val sb = new StringBuilder(); sb.append(\"y\").toString }\nb()"))
    assert(local.output.contains("y"), local.output)
  test("safe mode: Thread.sleep and sys.error are rejected"):
    assertFails(run("Thread.sleep(1)"), "safe code")
    assertFails(run("""sys.error("boom")"""), "safe code")
  test("safe mode: local var and immutable collections are fine"):
    val r = assertOk(run("def count(): Int = { var c = 0; for i <- 1 to 3 do c += i; c }\ncount()"))
    assert(r.output.contains("6"), r.output)
  test("safe mode: effects inside Option.foreach and match both work"):
    // Older nightlies rejected the effectful lambda; since 3.10.1 nightlies (2026-09) it compiles.
    assertOk(run("Some(1).foreach(println)"))
    assertOk(run("Some(1) match { case Some(v) => println(v); case _ => () }"))
  test("safe mode: top-level val of a capturing type needs an explicit type"):
    assertFails(run("""val handle = access("hello.txt")"""), "needs an explicit type")
    assertFails(run("""val printer = () => println("x")"""), "needs an explicit type")
    assertOk(run("""val handle: FileEntry^{fs} = access("hello.txt")"""))
    assertOk(run("""def printer(): Unit = println("x")"""))
  test("no safe mode: top-level var and mutable collections work, validator still applies"):
    assert(assertOk(nosafe.run("var counter = 0; counter += 1; counter")).output.contains("1"))
    assertOk(nosafe.run("val buf = scala.collection.mutable.ArrayBuffer[Int](); buf += 1; buf.size"))
    assertFails(nosafe.run("import java.io.File"), "file-io-java")
    assertFails(nosafe.run("""sys.props("user.home")"""), "sys-scala")
    assertFails(nosafe.run("import java.\n  io.File"), "file-io-java")
    assertFails(nosafe.run("""val h = s"${System.getProperty("user.home")}"; h"""), "sys-getprop")
    assertFails(nosafe.run("""val hiddenRuntime = Runtime; hiddenRuntime.rootIO"""), "atc-runtime")
    assertFails(nosafe.run("""import java.{io as jio}; new jio.File("/etc/passwd").exists"""), "import-alias")
    assertFails(nosafe.run("""Runtime.`rootIO`"""), "atc-runtime")
  test("a return inside a lambda is a compile error, and @nowarn cannot silence it"):
    val lambda = "def firstBig(xs: List[Int]): Int =\n  xs.foreach(x => if x > 1 then return x)\n  0"
    assertFails(run(lambda), "Non local returns are no longer supported")
    assertFails(run("@annotation.nowarn " + lambda), "nowarn")
    assertFails(nosafe.run(lambda), "Non local returns are no longer supported")
    // A return from the method itself stays an ordinary return.
    assertOk(run("def firstPlain(x: Int): Int =\n  if x > 1 then return x\n  0"))
    assert(assertOk(run("firstPlain(3)")).output.contains("3"))
  test("no safe mode: the library catch helpers are rejected before they run"):
    // allCatch would swallow the stop signal, and Breaks a control throwable from a callback.
    assertFails(nosafe.run("import scala.util.control.Exception.allCatch\nallCatch.opt(1)"), "catch-helper")
    assertFails(nosafe.run("scala.util.control.Breaks.breakable { 1 }"), "catch-helper")
    assertFails(nosafe.run("import scala.util.control.NonLocalReturns.*\nreturning { 1 }"), "catch-helper")
  test("no safe mode: :imports lacks the safe import"):
    assert(!assertOk(nosafe.run(":imports")).output.contains("experimental.safe"))
  test("no safe mode: capture checking still prevents leaks"):
    assertFails(nosafe.run("""val leaked = requestFiles("/tmp") { access("/tmp") }"""), "leak")

  // ── Timeouts and interruption ───────────────────────────────────

  test("execution timeout is reported and the session stays usable"):
    assertOk(quick.run("val beforeTimeout = 40"))
    val r = quick.run("""println("before the limit"); while true do ()""")
    assert(!r.success)
    assert(r.error.exists(_.contains("timed out")), r.error.toString)
    // What it printed before the limit reaches the model, without the stop signal's trace.
    assertEquals(r.output, "before the limit")
    assert(assertOk(quick.run("1 + 1")).output.contains("2"))
    assertOk(quick.run("val afterTimeout = beforeTimeout + 2"))
    assert(assertOk(quick.run("afterTimeout")).output.contains("42"))
  test("time spent waiting for the user is not counted against the timeout"):
    val outside = TestEnv.outsideDir("slow-user")
    envQuick.onRequest = _ =>
      envQuick.session.foreach(_.clock.pause())
      Thread.sleep(1500)
      envQuick.session.foreach(_.clock.resume())
    envQuick.decisions = List(Decision.AllowOnce)
    try
      val r = assertOk(quick.run(
        s"requestFiles(${envQuick.scalaString(outside)}, Access.Read, \"slow\") { read(${envQuick.scalaString(outside.resolve("o.txt"))}) }"
      ))
      assert(r.output.contains("slow-user"), r.output)
    finally envQuick.onRequest = _ => ()
  test("interrupt() aborts a running evaluation"):
    val env = TestEnv(prefix = "atc-repl-interrupt")
    val s = env.newSession(timeoutMs = Some(60000L))
    assertOk(s.run("val beforeInterrupt = 40"))
    assertFails(stopped(s, "while true do ()")(_.interrupt()))
    assert(assertOk(s.run("2 + 2")).output.contains("4"))
    assertOk(s.run("val afterInterrupt = beforeInterrupt + 2"))
    assert(assertOk(s.run("afterInterrupt")).output.contains("42"))

  test("interrupt() and the timeout stop tasks running in parallel"):
    val busy = "parallel(List(1, 2).map(i => () => { var n = 0L; while true do n += i; n }))"
    val env = TestEnv(prefix = "atc-repl-parallel")
    val s = env.newSession(timeoutMs = Some(60000L))
    assertFails(stopped(s, busy)(_.interrupt()))
    assert(assertOk(s.run("2 + 2")).output.contains("4"))
    val timedOut = quick.run(busy)
    assert(timedOut.error.exists(_.contains("timed out")), timedOut.toString)
    assert(assertOk(quick.run("3 + 3")).output.contains("6"))

  test("interrupt() and the timeout stop a loop inside Classified.map"):
    // `map` keeps what its callback throws, except the stop signal.
    val loop = "classify(1L).map(n => { var i = 0L; while true do i += n; i })"
    val interrupted = stopped(safe, loop)(_.interrupt())
    assert(interrupted.error.exists(_.contains("interrupted")), interrupted.toString)
    assert(assertOk(run("2 + 2")).output.contains("4"))
    val timedOut = quick.run(loop)
    assert(timedOut.error.exists(_.contains("timed out")), timedOut.toString)
    assert(assertOk(quick.run("3 + 3")).output.contains("6"))

  test("close() interrupts a running evaluation and refuses later runs"):
    val env = TestEnv(prefix = "atc-repl-close")
    val s = env.newSession(timeoutMs = Some(60000L))
    assertFails(stopped(s, "while true do ()")(_.close()))
    val after = s.run("1 + 1")
    assert(!after.success)
    assert(after.error.exists(_.contains("closed")), after.error.toString)

  test("time a command runs is not counted against the snippet's timeout"):
    // `exec` runs inside `whileCommandRuns`, which pauses the execution clock:
    // a 2 s command must fit a 1 s snippet budget.
    val sleep = ProcessFixture.command("sleep")
    val env = TestEnv(commands = List(ProcessFixture.pattern("sleep")), prefix = "atc-repl-cmdpause")
    val s = env.newSession(timeoutMs = Some(1000L))
    val r = assertOk(s.run(s"""exec(${ujson.write(sleep)}, List("2000")); println("woke")"""))
    assert(r.output.contains("woke"), r.output)
    assertEquals(env.commandsWrapped, 1) // it did go through the pausing hook

  // ── Output capture ──────────────────────────────────────────────

  test("printed output is bounded by the capture limit"):
    val r = assertOk(run("""println("x" * (ReplSessionMax + 100000)); ()""".replace(
      "ReplSessionMax",
      ReplSession.MaxOutputBytes.toString
    )))
    assert(r.output.endsWith(ReplSession.TruncationMarker.trim), r.output.takeRight(200))
    assert(r.output.length <= ReplSession.MaxOutputBytes + ReplSession.TruncationMarker.length + 16)
  test("leading whitespace of printed output is preserved (the UI matches it verbatim)"):
    val r = assertOk(run("""println("   indented"); ()"""))
    assert(r.output.startsWith("   indented"), r.output)
  test("echoed values are capped, printed output is not"):
    val r = assertOk(run("""val big = "x" * 5000"""))
    assert(r.output.contains("more characters not shown"), r.output.takeRight(200))
    assert(r.output.length < 5000, r.output.length.toString)
    val p = assertOk(run("""println("y" * 5000); ()"""))
    assert(p.output.count(_ == 'y') == 5000, p.output.length.toString)
  test("the echo cap never splits a surrogate pair and counts the hidden characters exactly"):
    // Default cap is 2000 chars; the echo `val emoji: String = "🙂🙂🙂…"` puts a pair's high half at
    // index 1999 (21-char prefix), so a naive cut would leave a lone surrogate.
    val r = assertOk(run("""val emoji = "\uD83D\uDE42" * 1200"""))
    val shown = r.output.takeWhile(_ != '…')
    assert(!Character.isHighSurrogate(shown.last), "cut inside a surrogate pair")
    val hidden = """\[(\d+) more characters""".r.findFirstMatchIn(r.output).map(_.group(1).nn.toInt).getOrElse(-1)
    val full = "val emoji: String = \"" + "\uD83D\uDE42" * 1200 + "\""
    assertEquals(shown.length + hidden, full.length)
  test("Java-returning calls bound to top-level vals do not warn about flexible types"):
    val r = assertOk(run("""val s = "abc".replace("a", "b")"""))
    assert(!r.output.contains("flexible type"), r.output)
    assert(!r.output.contains("warning"), r.output)
  test("output is reset between runs"):
    assertOk(run("""println("first")"""))
    val r = assertOk(run("""println("second")"""))
    assert(!r.output.contains("first"), r.output)

  // ── Preamble ────────────────────────────────────────────────────

  test("a broken preamble fails init loudly"):
    val env = TestEnv(prefix = "atc-repl-badpre")
    val e = intercept[IllegalStateException](env.newSession(preambleOverride = Some("val x: Int = \"no\"")))
    assert(e.getMessage.nn.contains("preamble"), e.getMessage)
  test("agent code cannot re-take the root capability"):
    // The validator blocks the name outright...
    assertFails(run("atc.lib.Runtime.rootIO"), "atc-runtime")
    // ...and the given `io` the preamble bound is the only IOCap in scope; there is no
    // other way to obtain one, so an effect always needs the single existing capability.
    assert(assertOk(run("""println("io works")""")).output.contains("io works"))

  // ── ExecutionResult / helpers (unit) ────────────────────────────

  test("ExecutionResult.render"):
    assertEquals(ExecutionResult(true, "").render, "(no output)")
    assertEquals(ExecutionResult(false, "").render, "(failed, no output)")
    assertEquals(ExecutionResult(true, "out").render, "out")
    assertEquals(ExecutionResult(false, "", Some("bad")).render, "ERROR: bad")
    assertEquals(ExecutionResult(false, "out", Some("bad")).render, "out\nERROR: bad")
  test("ExecutionResult.trimStackFrames"):
    val trace = """java.lang.RuntimeException: boom
      |  at rs$line$3$.f(rs$line$3:1)
      |  at atc.host.Host.exec(Host.scala:1)
      |  at java.base/java.lang.Thread.run(Thread.java:1)
      |  at scala.Function0.apply(Function0.scala:1)
      |  at dotty.tools.repl.Rendering.x(Rendering.scala:1)
      |  at sun.misc.X.y(X.java:1)
      |  at jdk.internal.X.y(X.java:1)
      |  ... 33 elided
      |  ... 2 more""".stripMargin
    assertEquals(
      ExecutionResult.trimStackFrames(trace),
      "java.lang.RuntimeException: boom\n  at rs$line$3$.f(rs$line$3:1)"
    )
  test("an exception thrown while binding a val, or by a later statement, fails the run"):
    assertFails(run("""val v: Int = throw new IllegalStateException("in val")"""), "in val")
    assertFails(run("def f(): Int = throw new IllegalStateException(\"in def\")\nval ok = 1\nf()"), "in def")
  test("output that merely looks like a stack trace does not fail the run"):
    assertOk(run("""println("java.lang.RuntimeException: boom\n  ... 33 elided")"""))
    assertOk(run("""println("java.lang.RuntimeException: boom\n  at rs$line$1$.x(rs$line$1:1)")"""))
  test("a closed session refuses to run"):
    val env = TestEnv(prefix = "atc-repl-closed")
    val s = env.newSession()
    s.close()
    assertFails(s.run("1 + 1"), "closed")
  test("BoundedOutputStream truncates and resets"):
    val b = ReplSession.BoundedOutputStream(10)
    b.write("hello".getBytes)
    assert(!b.truncated)
    b.write(" world!!".getBytes)
    assert(b.truncated)
    assertEquals(b.capturedString, "hello worl")
    b.write('x'.toInt)
    assertEquals(b.capturedString, "hello worl")
    b.resetCapture()
    assert(!b.truncated)
    assertEquals(b.capturedString, "")
    b.write('x'.toInt)
    assertEquals(b.capturedString, "x")
  test("the clock is reset for every run, including ones the validator rejects"):
    quick.clock.pause(); Thread.sleep(20); quick.clock.resume()
    assert(quick.clock.paused > 0)
    assertFails(quick.run("java.io.File"), "forbidden")
    assertEquals(quick.clock.paused, 0L)
  test("ExecutionClock accounts paused time; pauses nest, and a resume without a pause changes nothing"):
    val clock = ExecutionClock()
    assertEquals(clock.paused, 0L)
    clock.pause()
    Thread.sleep(50)
    assert(clock.paused >= 40_000_000L, clock.paused.toString)
    clock.resume()
    val after = clock.paused
    Thread.sleep(20)
    assertEquals(clock.paused, after) // not counting while running
    clock.resume()
    assertEquals(clock.paused, after)
    // The clock runs again only when the outermost pause ends.
    clock.pause()
    clock.pause()
    clock.resume()
    val open = clock.paused
    Thread.sleep(5)
    assert(clock.paused > open, "still paused after the inner resume")
    clock.resume()
    val closed = clock.paused
    Thread.sleep(5)
    assertEquals(clock.paused, closed)
    clock.reset()
    assertEquals(clock.paused, 0L)
