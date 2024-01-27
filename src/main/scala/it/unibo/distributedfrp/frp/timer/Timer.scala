package it.unibo.distributedfrp.frp.timer

import it.unibo.distributedfrp.frp.StreamExtension.Stream
import it.unibo.distributedfrp.frp.StreamSinkExtension.StreamSink
import it.unibo.distributedfrp.frp.timer.Timer.Tick

import java.util.concurrent.{ScheduledExecutorService, ScheduledFuture}
import scala.concurrent.duration.FiniteDuration

/**
 * A timer ticking after a certain [[FiniteDuration]].
 * A timer ticks only once unless reset.
 */
trait Timer:
  /** @return the [[FiniteDuration]] after which this [[Timer]] ticks when reset. */
  def duration: FiniteDuration

  /** @return a [[Stream Stream]] of the [[Tick Tick]]s of this [[Timer]]. */
  def ticks: Stream[Tick]

  /**
   * Reset the countdown until the next [[Tick Tick]].
   *
   * @return this.
   */
  def reset(): this.type

/** Companion object of [[Timer]]. */
object Timer:
  /** The tick of a [[Timer]]. */
  object Tick
  /** The type of a [[Tick Tick]]. */
  type Tick = Tick.type

  /**
   * @param duration the specified [[FiniteDuration]].
   * @param scheduler the [[ScheduledExecutorService]] used to tick the timer.
   * @return a new asynchronous [[Timer]] ticking after the specified [[FiniteDuration]]
   *         has elapsed. The [[Timer]] ticks only once unless reset.
   */
  def async(duration: FiniteDuration)(using scheduler: ScheduledExecutorService): Timer =
    BasicAsyncTimer(duration, scheduler)

  /** Basic implementation of an asynchronous [[Timer]]. */
  private case class BasicAsyncTimer(
    override val duration: FiniteDuration,
    scheduler: ScheduledExecutorService
  ) extends Timer:
    private val _ticks: StreamSink[Tick] = StreamSink[Tick]()
    private var _pendingTick: ScheduledFuture[?] = this.scheduleTick()

    override def ticks: Stream[Tick] = this._ticks

    override def reset(): this.type =
      synchronized {
        this._pendingTick.cancel(false)
        this._pendingTick = this.scheduleTick()
        this
      }

    /**
     * Schedule the next [[Tick Tick]] of this [[Timer]].
     *
     * @return a [[ScheduledFuture]] completing when this [[Timer]] has ticked.
     */
    private def scheduleTick(): ScheduledFuture[?] =
      scheduler.schedule[Unit](() => this._ticks.send(Tick), this.duration.length, this.duration.unit)
