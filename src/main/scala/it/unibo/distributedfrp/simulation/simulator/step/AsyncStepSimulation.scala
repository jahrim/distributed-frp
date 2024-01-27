package it.unibo.distributedfrp.simulation.simulator.step

import it.unibo.distributedfrp.simulation.simulator.Simulator
import it.unibo.distributedfrp.utils.Observables.Observable

import scala.concurrent.{ExecutionContext, Future}

/**
 * A mixin for providing the concept of asynchronous thread-safe
 * step-by-step simulation to a [[Simulator Simulator]].
 */
trait AsyncStepSimulation:
  self: Simulator with StepSimulation =>

  /** An asynchronous thread-safe [[StepSimulation]]. */
  trait AsyncStepSimulation[A] extends StepSimulation[A]:
    override def next(): Unit = asyncNext()(using ExecutionContext.parasitic)

    /**
     * Continue this [[Simulation Simulation]] on the given [[ExecutionContext]],
     * producing the next export.
     *
     * @param executor the specified [[ExecutionContext]].
     * @note multiple exports can be produced concurrently as long as critical races can
     *       be avoided. However, the order by which steps are produced becomes less meaningful
     *       if run concurrently.
     */
    def asyncNext()(using executor: ExecutionContext): Future[Unit]

    /**
     * @return an [[Observable Observable]] firing events each time this [[Simulation Simulation]]
     *         is ready to continue, producing the next export.
     */
    def ready: Observable[Unit]
