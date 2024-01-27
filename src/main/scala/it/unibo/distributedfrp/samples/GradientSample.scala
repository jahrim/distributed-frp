package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulation.environment.{Environment, EnvironmentWithTags}
import it.unibo.distributedfrp.simulation.incarnation.{CommonAlgorithms, CommonSensors, SimulationIncarnation}
import it.unibo.distributedfrp.simulation.simulator.legacy.LegacySimulator
import it.unibo.distributedfrp.utils.Liftable.*
import nz.sodium.Cell

@main def gradientSample(): Unit =
  given environment: EnvironmentWithTags = EnvironmentWithTags(Environment.manhattanGrid(5, 5))
  object Incarnation extends SimulationIncarnation with CommonSensors.Default with CommonAlgorithms
  val simulator = LegacySimulator(Incarnation)

  import simulator.incarnation.{*, given}

  environment.setTags(SourceTag -> Set(0), ObstacleTag -> Set(2, 7, 12))
  simulator.run {
    branch(isObstacle) {
      constant(-1.0)
    } {
      gradient(isSource)
    }
  }

  //  Thread.sleep(5000)
  //  println("===========================================================================")
  //  environment.setTag(SourceTag -> Set(4))
  //
  //  Thread.sleep(5000)
  //  println("===========================================================================")
  //  environment.setTag(ObstacleTag -> Set(3, 8, 13, 14))
