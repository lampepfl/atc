package atc.ui

import atc.Threads

import scala.concurrent.duration.FiniteDuration

/** How long a pop-up waits for someone to answer before it gives up on its own (config
  * `permissionTimeoutSeconds`). It runs from when it is made until the time is up or
  * [[stop]] stops it, as the first key the user presses in a menu does: someone is there,
  * so the pop-up waits for them from then on. One countdown serves the whole pop-up, so a
  * menu opened again after a stop waits too. `clock` reads nanoseconds, as
  * `System.nanoTime` does. */
private[ui] final class Countdown(val limit: FiniteDuration, clock: () => Long = () => System.nanoTime()):
  private val end = clock() + limit.toNanos
  @volatile private var stopped = false

  /** Stop counting: the pop-up waits for the user from now on. */
  def stop(): Unit = stopped = true

  def running: Boolean = !stopped

  /** Whether the time ran out before the countdown was stopped. */
  def expired: Boolean = !stopped && clock() - end >= 0

  /** Whole seconds left, rounded up: what a countdown shows. */
  def secondsLeft: Long = ((end - clock()).max(0L) + 999_999_999L) / 1_000_000_000L

  /** Run `read`, which blocks this thread reading a line, and interrupt this thread when the
    * time runs out first, as JLine's line reader is interrupted on Ctrl-C (it throws a
    * `UserInterruptException`). An interrupt that arrives after `read` returned is cleared,
    * so it cannot end a later read or wait. */
  def interrupting[T](read: => T): T =
    val reader = Thread.currentThread()
    val lock = Object()
    var reading = true
    var interrupted = false
    val watchdog = Threads.daemon("atc-countdown"): () =>
      lock.synchronized:
        while reading && !expired do lock.wait(Countdown.PollMillis)
        if reading && expired then
          interrupted = true
          reader.interrupt()
    watchdog.start()
    try read
    finally
      lock.synchronized:
        reading = false
        lock.notifyAll()
        if interrupted then Thread.interrupted()

private[ui] object Countdown:
  /** How often the watchdog of [[Countdown.interrupting]] looks at the clock. */
  private val PollMillis = 100L
