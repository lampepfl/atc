package atc

import atc.host.Host
import atc.perms.*
import atc.sandbox.{ExecutionResult, ReplSession}

/** Monitors at run time, through a real REPL: their schedule, what reaches the agent and the
  * user, their failures, their permissions and their end. What their callback may capture is
  * in [[CapabilitySuite]]. */
class MonitorSuite extends munit.FunSuite, ReplAssertions:
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private val env = TestEnv(commands = List(ProcessFixture.pattern("echo")), prefix = "atc-monitor")
  private lazy val session: ReplSession = env.newSession(mode = Mode.Full)
  private def run(code: String): ExecutionResult = session.run(code)

  override def beforeAll(): Unit =
    env.file("a.txt", "content")
    session

  override def afterEach(context: AfterEach): Unit =
    env.host.stopMonitors()
    env.monitorMessages.synchronized(env.monitorMessages.clear())
    env.monitorEvents.synchronized(env.monitorEvents.clear())

  override def afterAll(): Unit = session.close()

  private def messages(e: TestEnv = env): List[(Int, String, String)] =
    e.monitorMessages.synchronized(e.monitorMessages.toList)
  private def events(e: TestEnv = env): List[(Int, String)] = e.monitorEvents.synchronized(e.monitorEvents.toList)

  /** Wait until `condition` holds, failing after `ms`. */
  private def eventually(what: String, ms: Long = 20000)(condition: => Boolean)(using munit.Location): Unit =
    val end = System.nanoTime() + ms * 1000000L
    while !condition && System.nanoTime() < end do Thread.sleep(20)
    assert(condition, s"timed out waiting for $what")

  /** The id of the monitor named `name`, from `/monitors`. */
  private def idOf(name: String): String =
    env.host.monitorSummary.linesIterator.find(_.contains(s"  $name  ")).fold("")(_.takeWhile(_ != ' '))

  test("a monitor runs on its schedule, and what it sends reaches the agent"):
    assertOk(
      run("""addMonitor("reader", Schedule(everyMs = 1000, maxRuns = 2)) { () => send => send(read("a.txt")) }""")
    )
    eventually("two messages")(messages().size == 2)
    assertEquals(messages().map((_, name, text) => (name, text)), List.fill(2)(("reader", "content")))
    eventually("the end of its runs")(events().exists(_._2 == "finished after 2 runs"))
    assertEquals(env.host.monitorSummary, "no monitor is running")

  test("a monitor keeps the state its create made across runs"):
    assertOk(run(
      """addMonitor("counter", Schedule(everyMs = 1000, maxRuns = 3)) { () =>
        |  var n = 0
        |  send =>
        |    n += 1
        |    send(n.toString)
        |}""".stripMargin
    ))
    eventually("three runs")(messages().size == 3)
    assertEquals(messages().map(_._3), List("1", "2", "3"))

  test("a monitor tracks a file over time and reports each change once"):
    env.file("notes.md", "v1")
    assertOk(run(
      """addMonitor("notes", Schedule(everyMs = 1000)) { () =>
        |  var last = read("notes.md")
        |  send =>
        |    val now = read("notes.md")
        |    if now != last then
        |      send(s"notes.md is now $now")
        |      last = now
        |}""".stripMargin
    ))
    env.file("notes.md", "v2")
    eventually("the first change")(messages().nonEmpty)
    env.file("notes.md", "v3")
    eventually("the second change")(messages().size == 2)
    Thread.sleep(2500) // two more runs that see no change
    assertEquals(messages().map(_._3), List("notes.md is now v2", "notes.md is now v3"))

  test("create runs in the snippet that adds the monitor: what it throws fails that snippet"):
    assertFails(
      run("""addMonitor("broken", Schedule(everyMs = 1000)) { () => throw IllegalStateException("cannot start") }"""),
      "cannot start"
    )
    assertEquals(env.host.monitorSummary, "no monitor is running")

  test("a failed run is reported once, and three failed runs in a row stop the monitor"):
    assertOk(
      run("""addMonitor("failing", Schedule(everyMs = 1000)) { () => send => throw IllegalStateException("boom") }""")
    )
    eventually("the monitor to stop")(messages().size == 2)
    val texts = messages().map(_._3)
    assertEquals(texts.head, "[run 1 failed] java.lang.IllegalStateException: boom")
    assertEquals(texts(1), "[stopped after 3 failed runs in a row] java.lang.IllegalStateException: boom")
    assertEquals(env.host.monitorSummary, "no monitor is running")

  test("a stop flag that an interrupt left raised between evaluations does not stop a monitor"):
    assertOk(run(
      """addMonitor("loop", Schedule(everyMs = 1000, delayMs = 500, maxRuns = 1)) { () => send =>
        |  var n = 0
        |  while n < 100000 do n += 1
        |  send(n.toString)
        |}""".stripMargin
    ))
    session.interrupt() // Ctrl-C while no snippet runs: the REPL's stop flag stays raised
    eventually("the run's message")(messages().nonEmpty)
    assertEquals(messages().map(_._3), List("100000"))

  test("monitors lists the running monitors, and stopMonitor ends one"):
    assertOk(run("""val slow = addMonitor("slow", Schedule(everyMs = 60000, delayMs = 60000)) { () => send => () }"""))
    assert(assertOk(run("""monitors.map(_.name)""")).output.contains("slow"))
    assert(env.host.monitorSummary.contains("slow  (0 runs)"), env.host.monitorSummary)
    assertOk(run("""stopMonitor(slow)"""))
    assertEquals(env.host.monitorSummary, "no monitor is running")
    assertOk(run("""stopMonitor(slow)""")) // one that has ended stays so
    assertFails(run("""stopMonitor(999)"""), "there is no monitor m999")

  test("a monitor runs at most once a second, and at most eight run at once"):
    assertFails(run("""addMonitor("fast", Schedule(everyMs = 100)) { () => send => () }"""), "at least 1000")
    for i <- 1 to Host.MaxMonitors do
      assertOk(run(s"""addMonitor("m$i", Schedule(everyMs = 60000, delayMs = 60000)) { () => send => () }"""))
    assertFails(
      run("""addMonitor("one more", Schedule(everyMs = 60000, delayMs = 60000)) { () => send => () }"""),
      "already running",
    )

  test("the user stops a monitor by its number, or all of them"):
    assertOk(run("""addMonitor("first", Schedule(everyMs = 60000, delayMs = 60000)) { () => send => () }"""))
    assertOk(run("""addMonitor("second", Schedule(everyMs = 60000, delayMs = 60000)) { () => send => () }"""))
    assert(env.host.stopMonitorsByUser("").left.exists(_.contains("/monitors stop all")))
    assert(env.host.stopMonitorsByUser("m999").isLeft)
    val first = idOf("first")
    assertEquals(env.host.stopMonitorsByUser(first), Right(s"stopped $first (first)"))
    assert(!env.host.monitorSummary.contains("first") && env.host.monitorSummary.contains("second"))
    assert(env.host.stopMonitorsByUser("all").isRight)
    assertEquals(env.host.stopMonitorsByUser("all"), Left("no monitor is running"))

  test("what a monitor changes and runs is its own: an event for the user, no preview, no paused clock"):
    val wrapped = env.commandsWrapped
    val previews = env.fileChanges.synchronized(env.fileChanges.size)
    val echo = env.scalaString(ProcessFixture.command("echo", "ran"))
    assertOk(run(
      s"""addMonitor("writer", Schedule(everyMs = 1000, maxRuns = 1)) { () => send =>
         |  write("by-monitor.txt", "x")
         |  send(execOutput($echo).trim)
         |}""".stripMargin
    ))
    eventually("the run")(messages().nonEmpty)
    assertEquals(messages().map(_._3), List("ran"))
    assert(events().exists(_._2.startsWith("by-monitor.txt: created")), events().toString)
    assertEquals(env.fileChanges.synchronized(env.fileChanges.size), previews)
    assertEquals(env.commandsWrapped, wrapped)

  test("a monitor acts with the session's grants: a permission allowed once does not reach it"):
    val e = TestEnv(commands = Nil, prefix = "atc-monitor-grants")
    val s = e.newSession(mode = Mode.Full)
    try
      val echo = e.scalaString(ProcessFixture.command("echo", "hi"))
      val pattern = e.scalaString(ProcessFixture.pattern("echo"))
      e.decisions = List(Decision.AllowOnce, Decision.AllowSession)
      assertOk(s.run(s"""requestExec(List($pattern)) { execOutput($echo) }"""))
      assertOk(
        s.run(
          s"""addMonitor("once", Schedule(everyMs = 1000, maxRuns = 1)) { () => send => send(execOutput($echo)) }"""
        )
      )
      eventually("the refused run")(messages(e).nonEmpty)
      assert(messages(e).head._3.startsWith("[run 1 failed] java.lang.SecurityException"), messages(e).toString)
      assertOk(s.run(s"""requestExec(List($pattern)) { () }"""))
      assertOk(s.run(
        s"""addMonitor("session", Schedule(everyMs = 1000, maxRuns = 1)) { () => send => send(execOutput($echo).trim) }"""
      ))
      eventually("the permitted run")(messages(e).exists(_._2 == "session"))
      assertEquals(messages(e).filter(_._2 == "session").map(_._3), List("hi"))
    finally
      e.host.stopMonitors()
      s.close()

  test("what a monitor's run writes to System.err is dropped, since it may be the terminal"):
    // `printStackTrace` needs no capability; between evaluations `System.err` is the
    // process's own. A session wraps it once, so this one wraps the buffer.
    val before = System.err
    val buffer = java.io.ByteArrayOutputStream()
    System.setErr(java.io.PrintStream(buffer, true, "UTF-8"))
    val e = TestEnv(prefix = "atc-monitor-stderr")
    try
      val s = e.newSession(mode = Mode.Full)
      assertOk(s.run(
        """addMonitor("trace", Schedule(everyMs = 1000, delayMs = 500, maxRuns = 1)) { () => send =>
          |  RuntimeException("\u001b]0;title\u0007MONITOR-TRACE").printStackTrace()
          |  send("traced")
          |}""".stripMargin
      ))
      eventually("the run")(messages(e).nonEmpty)
      System.err.nn.println("APP-LINE")
      val written = buffer.toString("UTF-8")
      assert(!written.contains("MONITOR-TRACE"), written)
      assert(written.contains("APP-LINE"), written)
      s.close()
    finally System.setErr(before)

  test("a monitor ends with the REPL that compiled it"):
    val e = TestEnv(prefix = "atc-monitor-close")
    val s = e.newSession(mode = Mode.Full)
    assertOk(s.run("""addMonitor("orphan", Schedule(everyMs = 1000, delayMs = 500)) { () => send => send("tick") }"""))
    s.close()
    eventually("the monitor to end")(e.host.monitorSummary == "no monitor is running")
    assertEquals(messages(e), Nil)
