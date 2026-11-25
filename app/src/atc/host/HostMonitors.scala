package atc.host

import atc.lib.*

import java.util.Locale
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import scala.util.control.NonFatal

/** Monitors: agent code that runs in the background on a schedule and reports to the agent.
  * The callback's type allows only `io` and what derives from it, so it holds no `user` and no
  * capability of a `request*` scope; it acts with the base scope, the configured permissions
  * and the session grants. Each monitor has a thread of its own, so one that never returns
  * holds up no other. */
private[host] trait HostMonitors:
  self: Host =>

  /** The live monitors by id; ids are never reused within a host. */
  private val monitorsById = scala.collection.mutable.LinkedHashMap[Int, HostMonitors.Monitor]()
  private var nextMonitorId = 0

  /** The REPL evaluating agent code now, which a monitor added by that code keeps. */
  @volatile private[atc] var repl: MonitorRepl = MonitorRepl.Unattached

  def addMonitor(name: String, schedule: Schedule)(using UserIO, IOCap)(op: (String => Unit) => Unit): Int =
    if schedule.everyMs < Host.MinMonitorIntervalMs then
      throw IllegalArgumentException(
        s"addMonitor: everyMs must be at least ${Host.MinMonitorIntervalMs} (got ${schedule.everyMs})"
      )
    if schedule.delayMs < 0 then throw IllegalArgumentException(s"addMonitor: delayMs must not be negative")
    if schedule.maxRuns < 0 then throw IllegalArgumentException(s"addMonitor: maxRuns must not be negative")
    monitorsById.synchronized:
      if monitorsById.size >= Host.MaxMonitors then
        throw IllegalStateException(
          s"addMonitor: ${Host.MaxMonitors} monitors are already running (${monitorsById.keys.map(id => s"m$id").mkString(", ")}); stopMonitor one first"
        )
      nextMonitorId += 1
      val monitor = HostMonitors.Monitor(nextMonitorId, name, schedule.maxRuns, op, repl)
      monitorsById(monitor.id) = monitor
      monitor.executor.scheduleWithFixedDelay(
        () => run(monitor),
        schedule.delayMs,
        schedule.everyMs,
        TimeUnit.MILLISECONDS
      )
      monitor.id

  def monitors(using UserIO): List[MonitorInfo] = monitorsById.synchronized(monitorsById.values.toList.map(_.info))

  def stopMonitor(id: Int)(using UserIO): Unit =
    val (monitor, added) = monitorsById.synchronized((monitorsById.get(id), id >= 1 && id <= nextMonitorId))
    monitor match
      case Some(m) => end(m)
      case None => if !added then throw IllegalArgumentException(s"stopMonitor: there is no monitor m$id")

  /** Stop every monitor: the REPL that compiled them is going away, or ATC is. */
  private[atc] def stopMonitors(): Unit =
    monitorsById.synchronized(monitorsById.values.toList).foreach(end)

  /** `/monitors`: one line per monitor. */
  private[atc] def monitorSummary: String = monitorsById.synchronized:
    if monitorsById.isEmpty then "no monitor is running"
    else
      monitorsById.values
        .map: m =>
          val info = m.info
          val failed = info.lastError.fold("")(error => s"  last run failed: $error")
          s"m${info.id}  ${info.name}  (${info.runs} run${if info.runs == 1 then "" else "s"})$failed"
        .mkString("\n")

  /** `/monitors stop`: `m2`, `2`, or `all`. `Right` when monitors were stopped, `Left` when none was. */
  private[atc] def stopMonitorsByUser(ref: String): Either[String, String] =
    ref.trim.toLowerCase(Locale.ROOT) match
      case "" => Left("/monitors stop m<number> stops one monitor; /monitors stop all stops every one")
      case "all" =>
        val all = monitorsById.synchronized(monitorsById.values.toList)
        all.foreach(end)
        if all.isEmpty then Left("no monitor is running")
        else Right(s"stopped ${all.map(m => s"m${m.id}").mkString(", ")}")
      case value =>
        value.stripPrefix("m").toIntOption.flatMap(id => monitorsById.synchronized(monitorsById.get(id))) match
          case Some(m) =>
            end(m)
            Right(s"stopped m${m.id} (${m.name})")
          case None => Left(s"no running monitor '$ref' (see /monitors)")

  private def end(m: HostMonitors.Monitor): Unit =
    monitorsById.synchronized(monitorsById.remove(m.id))
    m.stopped = true
    // Interrupts a run in progress; a loop in agent code ends only with the REPL's stop flag.
    m.executor.shutdownNow()
    ()

  /** One run. Nothing may escape: the executor would cancel the schedule without a word. */
  private def run(m: HostMonitors.Monitor): Unit =
    if !m.stopped then
      if !m.repl.readyForMonitor() then end(m)
      else
        HostMonitors.current.set(m.id)
        // `None` when a stop signal cut the run short (the REPL's stop or `stopMonitor`): no failure.
        val outcome: Option[Option[Throwable]] =
          try
            m.op(text => if !m.stopped then output.monitorMessage(m.id, m.name, HostMonitors.capped(text)))
            Some(None)
          catch
            case _: ThreadDeath | _: InterruptedException => None
            case other: Throwable => Some(Some(other))
          finally
            HostMonitors.current.remove()
            Thread.interrupted()
        outcome.foreach: failure =>
          if !m.stopped then
            try settle(m, failure)
            catch case NonFatal(e) => atc.Debug.trace(e)

  private def settle(m: HostMonitors.Monitor, failure: Option[Throwable]): Unit =
    m.runs += 1
    failure match
      case None =>
        m.failures = 0
        m.lastError = None
      case Some(e) =>
        m.failures += 1
        val error = HostMonitors.describe(e)
        m.lastError = Some(error)
        if m.failures >= Host.MaxMonitorFailures then
          output.monitorMessage(m.id, m.name, s"[stopped after ${m.failures} failed runs in a row] $error")
          end(m)
        else if m.failures == 1 then output.monitorMessage(m.id, m.name, s"[run ${m.runs} failed] $error")
    if !m.stopped && m.maxRuns > 0 && m.runs >= m.maxRuns then
      output.monitorEvent(m.id, s"finished after ${m.runs} run${if m.runs == 1 then "" else "s"}")
      end(m)

private[atc] object HostMonitors:
  /** The monitor whose run the current thread serves, 0 for none. `parallel` hands it to its workers. */
  private[host] val current: ThreadLocal[Int] = ThreadLocal.withInitial(() => 0).nn

  private[host] final class Monitor(
    val id: Int,
    val name: String,
    val maxRuns: Int,
    val op: (String => Unit) => Unit,
    val repl: MonitorRepl,
  ):
    val executor: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor: runnable =>
      val thread = Thread(runnable, s"atc-monitor-m$id")
      thread.setDaemon(true)
      thread
    .nn
    @volatile var stopped = false
    @volatile var runs = 0
    @volatile var lastError: Option[String] = None
    /** Failed runs in a row; touched by the monitor's own thread only. */
    var failures = 0

    def info: MonitorInfo = MonitorInfo(id, name, runs, lastError)

  /** A message longer than this is cut, with a marker. */
  val MaxMessageChars: Int = 4000

  private def capped(text: String): String =
    if text.length <= MaxMessageChars then text
    else text.take(MaxMessageChars) + s" ... [${text.length - MaxMessageChars} more characters]"

  /** What a failed run threw. Its message is agent code, which may itself fail. */
  private def describe(e: Throwable): String =
    val message =
      try Option(e.getMessage).filter(_.nonEmpty)
      catch
        case _: ThreadDeath | _: InterruptedException => None
        case _: Throwable => None
    capped(e.getClass.getName + message.fold("")(": " + _))

/** The output port of a monitor's run. What it changes is shown as its own event, not in
  * the agent's open tool block; its commands are not shown live and do not pause the clock
  * of an evaluation that runs meanwhile. Process events are shown as for the agent. */
private[host] final class MonitorOutput(id: Int, ports: HostOutput) extends HostOutput:
  def print(agentText: String, userText: String): Unit = () // needs `UserIO`, which a monitor lacks
  override def fileChanged(change: FileChange): Unit = ports.monitorEvent(id, s"${change.path}: ${change.summary}")
  override def processStarted(id: Int, commandLine: String): Unit = ports.processStarted(id, commandLine)
  override def processInput(id: Int, text: String): Unit = ports.processInput(id, text)
  override def processExited(id: Int, exitCode: Int): Unit = ports.processExited(id, exitCode)
  override def monitorMessage(id: Int, name: String, text: String): Unit = ports.monitorMessage(id, name, text)
  override def monitorEvent(id: Int, text: String): Unit = ports.monitorEvent(id, text)
