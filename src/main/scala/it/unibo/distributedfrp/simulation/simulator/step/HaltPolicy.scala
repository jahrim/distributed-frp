package it.unibo.distributedfrp.simulation.simulator.step

import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.simulation.simulator.Simulator

import scala.util.Try

/**
 * A mixin for providing the concept of a policy for terminating
 * a step-by-step simulation to a [[Simulator Simulator]].
 */
trait HaltPolicy:
  self: Simulator with StepSimulation =>
  import incarnation.{*, given}

  /**
   * A policy for terminating a [[StepSimulation StepSimulation]].
   *
   * @tparam A the type of the results produced by the [[StepSimulation StepSimulation]].
   */
  type HaltPolicy[A] = StepSimulation[A] => Unit

  /** Companion object of [[HaltPolicy HaltPolicy]]. */
  object HaltPolicy:
    /**
     * @tparam A the type of the results produced by the [[StepSimulation StepSimulation]].
     * @return an [[HaltPolicy HaltPolicy]] that never stops a [[StepSimulation StepSimulation]].
     */
    def never[A]: HaltPolicy[A] = identity

    /**
     * @param policies the specified [[HaltPolicy HaltPolicy]].
     * @tparam A the type of the results produced by the [[StepSimulation StepSimulation]].
     * @return an [[HaltPolicy HaltPolicy]] that stops a [[StepSimulation StepSimulation]]
     *         whenever any of the specified [[HaltPolicy HaltPolicy]]s is triggered.
     */
    def combine[A](policies: HaltPolicy[A]*): HaltPolicy[A] = simulation =>
      policies.foreach(_.apply(simulation))

    /**
     * @param predicate the specified predicate.
     * @tparam A the type of the results produced by the [[StepSimulation StepSimulation]].
     * @return an [[HaltPolicy HaltPolicy]] that stops a [[StepSimulation StepSimulation]]
     *         when the state of the collective system satisfies the specified predicate.
     * @see [[StepSimulation.computedByAll]] for more information.
     */
    def haltWhen[A](predicate: CollectiveResultMap[A] => Boolean): HaltPolicy[A] = simulation =>
      simulation.computedByAll.filter(predicate(_)).listenOnce(_ => Try(simulation.stop()))

    /**
     * @param maxSteps the specified number of steps.
     * @tparam A the type of the results produced by the [[StepSimulation StepSimulation]].
     * @return an [[HaltPolicy HaltPolicy]] that stops a [[StepSimulation StepSimulation]]
     *         after the specified number of steps have been executed.
     */
    def haltAfter[A](maxSteps: Int): HaltPolicy[A] = simulation =>
      simulation.steps.zipWithIndex().filter(_._2 >= maxSteps - 1).listenOnce(_ => Try(simulation.stop()))

    /**
     * @tparam A the type of the results produced by the [[StepSimulation StepSimulation]].
     * @return an [[HaltPolicy HaltPolicy]] that stops a [[StepSimulation StepSimulation]]
     *         the first time a step doesn't produce any device exports when executed.
     */
    def haltOnVainStep[A]: HaltPolicy[A] = simulation =>
      simulation.steps.filter(_.isEmpty).listenOnce(_ => Try(simulation.stop()))