package it.unibo.distributedfrp.utils

import scala.concurrent.duration.*

/** A stopwatch that can be used to measure the time elapsed between several events. */
trait StopWatch:
  /**
   * Start the tracking of time from this point forward.
   *
   * @return this.
   */
  def start(): this.type

  /**
   * Pause the tracking of time.
   *
   * @return this.
   * @throws IllegalStateException if the [[StopWatch]] has not been started.
   */
  def pause(): this.type

  /**
   * Register the [[FiniteDuration]] elapsed since the start of this [[StopWatch]]
   * as a lap of this [[StopWatch]].
   *
   * @return the [[FiniteDuration]] elapsed since the start of this [[StopWatch]].
   * @throws IllegalStateException if the [[StopWatch]] has not been started.
   */
  def lap(): FiniteDuration

  /**
   * @return the laps registered in this [[StopWatch]].
   * @see [[lap]] for more information.
   */
  def laps: Seq[FiniteDuration]

  /**
   * Reset this [[StopWatch]], stopping the tracking of time and removing all
   * registered laps.
   *
   * @return this.
   */
  def reset(): this.type

/** Companion object of [[StopWatch]]. */
object StopWatch:
  /**
   * @param clock the given [[Clock]].
   * @return a new thread-safe [[StopWatch]] keeping track of time using the
   *         given [[Clock]].
   */
  def basic(using clock: Clock): StopWatch = BasicStopWatch(clock)

  /** Basic implementation of [[StopWatch]]. */
  private case class BasicStopWatch(clock: Clock) extends StopWatch:
    private var _startTime: Option[Long] = None
    private var _pauseLap: Option[Long] = None
    private var _laps: Seq[FiniteDuration] = Seq()

    override def start(): this.type =
      synchronized(this._startTime = Some(clock.nanos() - this._pauseLap.getOrElse(0L)))
      this

    override def pause(): this.type =
      synchronized {
        this._pauseLap = Some(this.lapNanos())
        this._startTime = None
      }
      this

    override def lap(): FiniteDuration =
      val nextLap: FiniteDuration = Duration(this.lapNanos(), NANOSECONDS)
      synchronized(this._laps = this._laps :+ nextLap)
      nextLap

    override def laps: Seq[FiniteDuration] = synchronized(this._laps)

    override def reset(): this.type =
      synchronized {
        this._startTime = None
        this._pauseLap = None
        this._laps = Seq()
      }
      this

    /** As [[lap]], but returns the duration in nanoseconds. */
    private def lapNanos(): Long =
      synchronized {
        this._startTime match
          case Some(start) => clock.nanos() - start
          case None => throw IllegalStateException("Cannot measure time if the stopwatch has not been started.")
      }
