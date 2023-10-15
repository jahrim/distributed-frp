package it.unibo.distributedfrp.frp.timer

import java.util.concurrent.{Executors, ScheduledExecutorService}
import scala.concurrent.duration.FiniteDuration

@FunctionalInterface
/** A factory for [[Timer]]s. */
trait TimerFactory[T <: Timer]:
  /**
   * @param duration the specified [[FiniteDuration]].
   * @return a new [[Timer]] ticking after the specified [[FiniteDuration]].
   */
  def create(duration: FiniteDuration): T

/** Companion object of [[TimerFactory]]. */
object TimerFactory:
  /** The number of [[Thread]]s in the thread pool of the global scheduler. */
  private val DefaultSchedulerPoolSize: Int = Runtime.getRuntime.availableProcessors + 1
  /** The default scheduler used for creating [[Timer]]s. */
  private val DefaultScheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(DefaultSchedulerPoolSize)

  /** @return a new [[TimerFactory]] that creates [[Timer]]s as for [[Timer.apply]]. */
  def async(using scheduler: ScheduledExecutorService = DefaultScheduler): TimerFactory[Timer] = Timer.async(_)
