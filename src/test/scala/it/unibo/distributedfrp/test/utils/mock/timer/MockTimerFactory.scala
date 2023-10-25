package it.unibo.distributedfrp.test.utils.mock.timer

import it.unibo.distributedfrp.frp.StreamExtension.Stream
import it.unibo.distributedfrp.frp.StreamSinkExtension.StreamSink
import it.unibo.distributedfrp.frp.timer.Timer.Tick
import it.unibo.distributedfrp.frp.timer.{Timer, TimerFactory}
import it.unibo.distributedfrp.test.utils.mock.clock.MockClockScheduler
import it.unibo.distributedfrp.test.utils.mock.clock.MockClockScheduler.*
import nz.sodium

import scala.concurrent.duration.FiniteDuration

/** A [[TimerFactory]] for [[Timer]]s ticking on the timeline of a [[MockClockScheduler]]. */
object MockTimerFactory:
  /**
   * @param scheduler the given [[MockClockScheduler]].
   * @return a new [[TimerFactory]] that creates [[Timer]]s ticking
   *         on the timeline of the given [[MockClockScheduler]].
   */
  def basic(using scheduler: MockClockScheduler): TimerFactory[Timer] =
    MockTimer(_, scheduler)

  /** A [[Timer]] that ticks on the timeline of a [[MockClockScheduler]]. */
  private case class MockTimer(
    override val duration: FiniteDuration,
    scheduler: MockClockScheduler
  ) extends Timer:
    private val _ticks: StreamSink[Tick] = sodium.StreamSink[Tick]()
    private var _pendingTick: PendingTask[?] = this.scheduleTick()
    override def ticks: Stream[Tick] = this._ticks
    override def reset(): this.type =
      synchronized {
        scheduler.cancel(this._pendingTick)
        this._pendingTick = this.scheduleTick()
        this
      }

    /** Schedule the next [[Tick Tick]] of this [[Timer]]. */
    private def scheduleTick(): PendingTask[?] =
      scheduler.scheduleAfter(duration){ this._ticks.send(Tick) }
