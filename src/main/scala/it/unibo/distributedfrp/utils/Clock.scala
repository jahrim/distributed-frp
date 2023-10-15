package it.unibo.distributedfrp.utils

import scala.concurrent.duration.*

/** A clock keeping track of the time in a specific timeline. */
@FunctionalInterface
trait Clock:
  /**
   * @return the number of nanoseconds elapsed since the origin of the
   *         timeline tracked by this [[Clock]].
   */
  def nanos(): Long

  /**
   * @return the [[FiniteDuration]] elapsed since the origin of the
   *         timeline tracked by this [[Clock]].
   */
  def time: FiniteDuration = Duration(this.nanos(), NANOSECONDS)

/** Companion object of [[Clock]]. */
object Clock:
  /**
   * An high-resolution [[Clock]] based on the time source
   * of the JVM, as in [[java.lang.System.nanoTime]].
   */
  val SystemClock: Clock = () => System.nanoTime
