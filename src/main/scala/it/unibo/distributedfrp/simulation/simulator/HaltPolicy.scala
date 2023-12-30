package it.unibo.distributedfrp.simulation.simulator

import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.frp.FiniteStreamExtension.*

import scala.concurrent.duration.FiniteDuration

/**
 * A policy that describes when to stop the execution of a flow.
 *
 * @tparam E the type of results computed by the flow.
 */
@FunctionalInterface
trait HaltPolicy[E] extends Function[Stream[E], FiniteStream[E]]

/** Companion object of [[HaltPolicy HaltPolicy]]. */
object HaltPolicy:
  /** @return an [[HaltPolicy HaltPolicy]] that never stops the execution of a flow. */
  def none[E]: HaltPolicy[E] = _.finite

  /**
   * @param interruptStream the specified [[Stream Stream]].
   * @return an [[HaltPolicy HaltPolicy]] that stops the execution of a flow when
   *         the specified [[Stream Stream]] fires an event.
   */
  def haltByInterrupt[E](interruptStream: Stream[?]): HaltPolicy[E] =
    _.finite.interruptBy(interruptStream)

  /**
   * @param duration the specified [[FiniteDuration]].
   * @return an [[HaltPolicy HaltPolicy]] that stops the execution of a flow after
   *         the specified [[FiniteDuration]].
   */
  def haltAfter[E](duration: FiniteDuration): HaltPolicy[E] =
    _.finite.interruptAfter(duration)

  /**
   * @param duration the specified [[FiniteDuration]].
   * @return an [[HaltPolicy HaltPolicy]] that stops the execution of a flow after no
   *         computation has been performed for the specified [[FiniteDuration]].
   */
  def haltAfterInactivityOf[E](duration: FiniteDuration): HaltPolicy[E] =
    _.finite.interruptAfterInactivityOf(duration)