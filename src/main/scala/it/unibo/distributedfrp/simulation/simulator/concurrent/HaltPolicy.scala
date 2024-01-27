package it.unibo.distributedfrp.simulation.simulator.concurrent

import it.unibo.distributedfrp.frp.FiniteStreamExtension.*
import it.unibo.distributedfrp.simulation.simulator.Simulator

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/**
 * A mixin for providing the concept of a policy for terminating
 * a concurrent simulation to a [[Simulator Simulator]].
 */
trait HaltPolicy:
  self: Simulator =>
  import incarnation.{*, given}

  /**
   * A policy for terminating a concurrent [[Simulation Simulation]].
   *
   * @tparam A the type of the results produced by the [[Simulation Simulation]].
   */
  type HaltPolicy[A] = Simulation[A] => Unit

  /** Companion object of [[HaltPolicy HaltPolicy]]. */
  object HaltPolicy:
    /**
     * @tparam A the type of the results produced by the [[Simulation Simulation]].
     * @return an [[HaltPolicy HaltPolicy]] that never stops a concurrent
     *         [[Simulation Simulation]].
     */
    def never[A]: HaltPolicy[A] = identity

    /**
     * @param policies the specified [[HaltPolicy HaltPolicy]].
     * @tparam A the type of the results produced by the [[Simulation Simulation]].
     * @return an [[HaltPolicy HaltPolicy]] that stops a concurrent [[Simulation Simulation]]
     *         whenever any of the specified [[HaltPolicy HaltPolicy]]s is triggered.
     */
    def combine[A](policies: HaltPolicy[A]*): HaltPolicy[A] = simulation =>
      policies.foreach(_.apply(simulation))

    /**
     * @param predicate the specified predicate.
     * @tparam A the type of the results produced by the [[Simulation Simulation]].
     * @return an [[HaltPolicy HaltPolicy]] that stops a concurrent [[Simulation Simulation]]
     *         when an event of the simulation satisfies the specified predicate.
     * @see [[Simulation.computedByAll]] for more information about the events of a simulation.
     */
    def haltWhen[A](predicate: CollectiveResultMap[A] => Boolean): HaltPolicy[A] = simulation =>
      simulation.computedByAll.filter(predicate(_)).listenOnce(_ => Try(simulation.stop()))

    /**
     * @param duration the specified [[FiniteDuration]].
     * @tparam A the type of the results produced by the [[Simulation Simulation]].
     * @return an [[HaltPolicy HaltPolicy]] that stops a concurrent [[Simulation Simulation]]
     *         after the specified [[FiniteDuration]] since the start of the simulation.
     */
    def haltAfterDurationOf[A](duration: FiniteDuration): HaltPolicy[A] = simulation =>
      simulation
        .computedByAll
        .finite
        .interruptAfter(duration)
        .termination
        .onComplete(_ => Try(simulation.stop()))(using ExecutionContext.parasitic)

    /**
     * @param duration the specified [[FiniteDuration]].
     * @tparam A the type of the results produced by the [[Simulation Simulation]].
     * @return an [[HaltPolicy HaltPolicy]] that stops a concurrent [[Simulation Simulation]]
     *         after the specified [[FiniteDuration]] since the last event of the simulation.
     * @see [[Simulation.computedByAll]] for more information about the events of a simulation.
     */
    def haltAfterInactivityOf[A](duration: FiniteDuration): HaltPolicy[A] = simulation =>
      simulation
        .computedByAll
        .finite
        .interruptAfterInactivityOf(duration)
        .termination
        .onComplete(_ => Try(simulation.stop()))(using ExecutionContext.parasitic)
