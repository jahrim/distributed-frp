package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulation.{AggregateProgramSimulator, Environment}
import it.unibo.distributedfrp.utils.Liftable.lift

@main def loopSample(): Unit =
  val environment = Environment.manhattanGrid(2, 1)
  val simulator = new AggregateProgramSimulator(environment)

  import simulator.SimulationIncarnation._
  import simulator.SimulationIncarnation.given

  simulator.run {
    loop(0) { x => x.map(_ + 1) }
  }
