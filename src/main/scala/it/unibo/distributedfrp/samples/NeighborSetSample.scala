package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulated.{AggregateProgramSimulator, Environment}
import it.unibo.distributedfrp.utils.Lift._

@main def neighborSetSample(): Unit =
  val environment = Environment.grid(2, 2)
  val simulator = new AggregateProgramSimulator(environment)

  import simulator.SimulationIncarnation._
  import simulator.SimulationIncarnation.given

  simulator.run {
    nbr(mid).map(_.toSet)
  }

