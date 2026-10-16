package atc.host

import atc.lib.Classified

import scala.util.{Failure, Success, Try}

/** Stores a classified value or the failure of the computation that produced it.
  * Capture checking enforces callback purity in the agent-facing API.
  * A callback failure stays inside the classified value whatever its type, fatal errors
  * and control throwables (a non-local `return`, `boundary.break`) included, so no catch
  * or `finally` in agent code can observe a secret-dependent failure. Only the stop
  * signals propagate, so that an interrupt or a timeout still ends the evaluation. */
final class ClassifiedImpl[+T](val value: Try[T]) extends Classified[T]:
  def map[B](op: T => B): Classified[B] = ClassifiedImpl(value.flatMap(v => ClassifiedImpl.attempt(op(v))))
  def flatMap[B](op: T => Classified[B]): Classified[B] =
    ClassifiedImpl(value.flatMap(v => ClassifiedImpl.attempt(op(v)).flatMap(ClassifiedImpl.unwrap)))
  override def toString: String = "Classified(***)"

object ClassifiedImpl:
  def wrap[T](value: T): Classified[T] = ClassifiedImpl(Success(value))

  /** Run agent code on classified data, keeping everything it throws in the result
    * except the stop signals: the sandbox's `ThreadDeath` and `InterruptedException`.
    * Agent code cannot name either type; they come from a stop request, not from the
    * data. The interrupt status needs no repair, since the exception is rethrown. */
  def attempt[B](op: => B): Try[B] =
    try Success(op)
    catch
      case stop @ (_: ThreadDeath | _: InterruptedException) => throw stop
      case other: Throwable => Failure(other)

  def unwrap[T](c: Classified[T]): Try[T] = c match
    case impl: ClassifiedImpl[T] @unchecked => impl.value
    case other => throw SecurityException(s"Unknown Classified implementation: ${other.getClass.getName}")

  /** Read the value out, for host code and tests that are allowed to see it.
    * A failed computation raises a sanitized error instead of the original
    * exception, which may quote the confidential value (a pure `map` lambda can
    * throw), so it must never reach the agent. */
  def get[T](c: Classified[T]): T =
    unwrap(c).getOrElse:
      throw IllegalStateException(
        "The classified value is the result of a failed computation; its error is confidential (println it to let the user see it)."
      )
