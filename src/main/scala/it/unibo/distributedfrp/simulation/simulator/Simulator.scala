package it.unibo.distributedfrp.simulation.simulator

import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation
import it.unibo.distributedfrp.simulation.simulator.Simulator.Components

/** A simulator capable of configuring simulations for a specific [[SimulationIncarnation]]. */
trait Simulator extends Components:
  /**
   * The configuration of a simulation in this [[Simulator]].
   *
   * @tparam A the type of results produced by the simulation.
   */
  type Configuration[A] <: SimulationConfiguration[A]
  /**
   * The type of the [[SimulationIncarnation]] for which this [[Simulator]]
   * is capable of configuring [[Simulation Simulation]]s.
   */
  type Incarnation <: SimulationIncarnation
  /**
   * The [[SimulationIncarnation]] for which this [[Simulator]] is capable of
   * configuring [[Simulation Simulation]]s.
   */
  val incarnation: Incarnation

  /**
   * Configure a new [[Simulation Simulation]] for executing the specified [[Flow]].
   *
   * @param flow          the specified [[incarnation.Flow Flow]].
   * @param configuration the specified [[Configuration Configuration]].
   * @tparam A the type of results produced by the specified [[incarnation.Flow Flow]].
   * @return a new [[Simulation Simulation]] for executing the specified [[Flow]].
   */
  def simulation[A](flow: incarnation.Flow[A])(using configuration: Configuration[A]): Simulation[A]

/** Companion object of [[Simulator]]. */
object Simulator:
  /**
   * A mixin that provides a specific [[SimulationIncarnation]]
   * to a [[Simulator]].
   */
  trait WithIncarnation[I <: SimulationIncarnation](simulationIncarnation: I):
    self: Simulator =>
    override type Incarnation = I
    override val incarnation: Incarnation = simulationIncarnation

  /** A mixin providing the components of a [[Simulator]]. */
  trait Components
    extends SimulationConfigurationComponent
       with SimulationComponent:
    self: Simulator =>