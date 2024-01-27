package it.unibo.distributedfrp.simulation.simulator.step

import it.unibo.distributedfrp.frp.StreamExtension.Stream
import it.unibo.distributedfrp.simulation.simulator.Simulator

/** A mixin for providing the concept of step-by-step simulation to a [[Simulator Simulator]]. */
trait StepSimulation:
  self: Simulator =>
  import incarnation.{*, given}

  /** A [[Simulation Simulation]] that can be executed step by step by the user. */
  trait StepSimulation[A] extends Simulation[A]:
    /** Continue this [[Simulation Simulation]] producing the next export, if any. */
    def next(): Unit
    /**
     * Repeat [[next]] for the specified number of steps.
     *
     * @param steps the specified number of steps.
     */
    final def next(steps: Int): Unit = (0 until steps).foreach(_ => next())

    /**
     * @return a [[Stream Stream]] of the steps executed in this simulation, containing
     *         the device exports produced during each step, if any.
     * @note a step is executed each time the [[StepSimulation.next next]] method of the
     *       simulation is called, regardless of whether a device generated a new event
     *       or not.
     */
    def exportedSteps: Stream[CollectiveExportMap[A]]
    /**
     * As [[exportedSteps]], but the [[Stream Stream]] contains the device results produced
     * during each step instead of the device exports.
     */
    final def steps: Stream[CollectiveResultMap[A]] = this._cachedSteps
    private lazy val _cachedSteps: Stream[CollectiveResultMap[A]] = this.exportedSteps.map(_.map(_ -> _.root))
    override def exported: Stream[CollectiveExportMap[A]] = this._cachedExported
    private lazy val _cachedExported: Stream[CollectiveExportMap[A]] = this.exportedSteps.filter(_.nonEmpty)
