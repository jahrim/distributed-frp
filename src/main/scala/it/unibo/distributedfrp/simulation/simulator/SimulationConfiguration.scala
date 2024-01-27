package it.unibo.distributedfrp.simulation.simulator

/**
 * A mixin for providing the concept of configuration
 * for a simulation to a [[Simulator Simulator]].
 */
trait SimulationConfiguration:
  self: Simulator =>

  import incarnation.{*, given}

  /**
   * The configuration of a [[Simulation Simulation]].
   *
   * @tparam A the type of results produced by the simulation.
   */
  trait SimulationConfiguration[A]:
    /** The [[Environment Environment]] of the simulation. */
    def environment: Environment
