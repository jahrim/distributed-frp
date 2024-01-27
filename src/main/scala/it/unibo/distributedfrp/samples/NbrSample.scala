package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulation.environment.{Environment, EnvironmentWithTags}
import it.unibo.distributedfrp.simulation.incarnation.{CommonSensors, SimulationIncarnation}
import it.unibo.distributedfrp.simulation.simulator.legacy.LegacySimulator

@main def nbrSample(): Unit =
  given environment: EnvironmentWithTags = EnvironmentWithTags(Environment.euclideanGrid(2, 2))
  object Incarnation extends SimulationIncarnation with CommonSensors.Default
  val simulator = new LegacySimulator(Incarnation)

  import simulator.incarnation.{*, given}

  simulator.run {
    nbr(mid).toSet
  }

