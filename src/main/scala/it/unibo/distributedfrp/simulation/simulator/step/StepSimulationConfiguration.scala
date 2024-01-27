package it.unibo.distributedfrp.simulation.simulator.step

import it.unibo.distributedfrp.simulation.simulator.Simulator
import it.unibo.distributedfrp.utils.Logger

/**
 * A mixin for providing the concept of configuration for a step-by-step
 * simulation to a [[Simulator Simulator]].
 */
trait StepSimulationConfiguration:
  self: Simulator with StepSimulation with HaltPolicy =>
  import incarnation.{*, given}

  /**
   * The [[SimulationConfiguration SimulationConfiguration]] of a [[StepSimulation StepSimulation]].
   *
   * @param environment the [[Environment Environment]] of the simulation.
   * @param haltPolicy  the [[HaltPolicy HaltPolicy]] deciding when the simulation will be stopped.
   *                    If [[HaltPolicy.never none]] is provided, the user will have to stop the
   *                    simulation manually. Defaults to: [[HaltPolicy.never none]].
   * @param logger      the [[Logger]] used within the simulation. Defaults to: [[Logger.NoOperation]].
   * @tparam A the type of results produced by the simulation.
   */
  case class StepSimulationConfiguration[A](
    override val environment: Environment,
    haltPolicy: HaltPolicy[A] = HaltPolicy.never[A],
    logger: Logger = Logger.NoOperation
  ) extends SimulationConfiguration[A]
