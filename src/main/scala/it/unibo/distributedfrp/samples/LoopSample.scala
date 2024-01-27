package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulation.environment.{Environment, EnvironmentWithTags}
import it.unibo.distributedfrp.simulation.incarnation.{CommonSensors, SimulationIncarnation}
import it.unibo.distributedfrp.simulation.simulator.legacy.LegacySimulator
import it.unibo.distributedfrp.utils.Liftable.*
import nz.sodium.CellSink

@main def loopSample(): Unit =
  given environment: EnvironmentWithTags = EnvironmentWithTags(Environment.singleNode)
  object Incarnation extends SimulationIncarnation with CommonSensors.Default
  val simulator = LegacySimulator(Incarnation)

  import simulator.incarnation.{*, given}

  simulator.run {
    loop(0) { x => x.map(_ + 1).map(math.min(_, 100)) }
  }
