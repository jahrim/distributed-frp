package it.unibo.distributedfrp.utils.mock

import it.unibo.distributedfrp.utils.Clock

import scala.concurrent.duration.*

/** A thread-safe [[Clock]] whose time can be set for testing. */
class MockClock() extends Clock:
  private var _nanos: Long = MockClock.Origin.toNanos
  override def nanos(): Long = synchronized(this._nanos)

  /**
   * Set the current time of this [[Clock]] so that the
   * specified [[FiniteDuration]] has elapsed since the
   * origin of the timeline.
   *
   * @param time the specified [[FiniteDuration]].
   */
  def setTime(time: FiniteDuration): Unit = synchronized(this._nanos = time.toNanos)

  /**
   * Forward the current time of this [[Clock]] by the
   * specified [[FiniteDuration]].
   *
   * @param duration the specified [[FiniteDuration]].
   */
  def forwardTime(duration: FiniteDuration): Unit = this.setTime(this.time + duration)

/** Companion object of [[MockClock]]. */
object MockClock:
  /** The default origin of time for the timelines tracked by [[MockClock]]s. */
  val Origin: FiniteDuration = 0L.nanoseconds