package atc

import atc.perms.Decision
import atc.sandbox.ExecutionResult

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

/** Run separately as well as in the full suite: other compiler sessions can populate
  * Scala's shared wrapper-name table and hide a missing index after interruption. */
class ReplInterruptionSuite extends munit.FunSuite:
  test("a pattern that takes too long to match ends with the timeout, and the session goes on"):
    // Matching is host code, which the stop flag does not reach; the interrupt does.
    val env = TestEnv(prefix = "atc-slow-pattern")
    env.file("a" * 200, "")
    env.file("slow.txt", "a" * 200 + "\n")
    val session = env.newSession(timeoutMs = Some(1500L))
    try
      for code <- List("find(\".\", \"" + "*a" * 14 + "*b\")", "grep(\"slow.txt\", \"" + ".*a" * 14 + ".*b\")") do
        val slow = session.run(code)
        assert(!slow.success, code)
        assert(slow.render.contains("timed out"), slow.render)
        assert(!slow.render.contains("could not be stopped"), slow.render)
      val next = session.run("1 + 1")
      assert(next.success && next.output.contains("2"), next.render)
    finally session.close()

  test("an interrupt returns although the evaluation is in host code that ignores it"):
    val env = TestEnv(prefix = "atc-interrupt-stuck")
    val session = env.newSession()
    val entered = CountDownLatch(1)
    val release = CountDownLatch(1)
    env.decisions = List(Decision.AllowOnce)
    env.onRequest = _ =>
      entered.countDown()
      var released = false
      while !released do
        try released = release.await(10, TimeUnit.SECONDS)
        catch case _: InterruptedException => ()
    val result = AtomicReference[ExecutionResult]()
    val caller = Thread(() => result.set(session.run("requestExec(List(\"test-stuck\"), \"stuck test\") { 1 }")))
    caller.setDaemon(true)
    try
      caller.start()
      assert(entered.await(10, TimeUnit.SECONDS), "evaluation did not reach the permission boundary")
      session.interrupt()
      caller.join(5000)
      assert(!caller.isAlive, "the interrupt did not return within the stop grace")
      val stuck = result.get().nn
      assert(!stuck.success)
      assert(stuck.render.contains("interrupted by the user"), stuck.render)
      assert(stuck.render.contains("could not be stopped"), stuck.render)
    finally
      release.countDown()
      session.close()

  test("interruption preserves earlier definitions and permits new bindings"):
    val env = TestEnv(prefix = "atc-interrupt-state")
    val session = env.newSession()
    val entered = CountDownLatch(1)
    env.decisions = List(Decision.AllowOnce)
    env.onRequest = _ => entered.countDown()
    val result = AtomicReference[ExecutionResult]()
    val caller = Thread(() =>
      result.set(session.run("requestExec(List(\"test-loop\"), \"interrupt test\") { while true do () }"))
    )
    caller.setDaemon(true)
    try
      assert(session.run("val before = 40").success)
      caller.start()
      assert(entered.await(10, TimeUnit.SECONDS), "evaluation did not reach the permission boundary")
      session.interrupt()
      caller.join(5000)
      assert(!caller.isAlive, "evaluation did not stop")
      assert(!result.get().nn.success)
      val next = session.run("val after = before + 2")
      assert(next.success, next.render)
      val value = session.run("after")
      assert(value.success && value.output.contains("42"), value.render)
    finally session.close()
