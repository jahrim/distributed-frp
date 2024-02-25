package it.unibo.distributedfrp.simulation.simulator.concurrent

import it.unibo.distributedfrp.simulation.simulator.Simulator
import it.unibo.distributedfrp.utils.Logger

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/**
 * A mixin for providing the concept of configuration for concurrent
 * simulation to a [[Simulator Simulator]].
 */
trait ConcurrentSimulationConfigurationComponent:
  self: Simulator with HaltPolicyComponent =>
  import incarnation.{*, given}

  /**
   * The [[SimulationConfiguration SimulationConfiguration]] of a concurrent [[Simulation Simulation]].
   *
   * @param environment the [[Environment Environment]] of the simulation.
   * @param haltPolicy  the [[HaltPolicy HaltPolicy]] deciding when the simulation will be stopped.
   *                    If [[HaltPolicy.never none]] is provided, the user will have to stop the
   *                    simulation manually. Defaults to: [[HaltPolicy.never none]].
   * @param executor    the [[ExecutionContext]] where the simulation will be executed.
   * @param logger      the [[Logger]] used within the simulation. Defaults to: [[Logger.NoOperation]].
   * @tparam A the type of results produced by the simulation.
   */
  case class ConcurrentSimulationConfiguration[A](
    override val environment: Environment,
    haltPolicy: HaltPolicy[A] = HaltPolicy.never[A],
    executor: ExecutionContext = ExecutionContext.global,
    logger: Logger = Logger.NoOperation,
  ) extends SimulationConfiguration[A]
