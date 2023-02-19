package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulated.{AggregateProgramSimulator, Environment}
import it.unibo.distributedfrp.utils.Lift.lift

@main def loopSample(): Unit =
  val environment = Environment.grid(2, 1)
  val simulator = new AggregateProgramSimulator(environment)

  import simulator.SimulationIncarnation._
  import simulator.SimulationIncarnation.given

  simulator.run {
    loop(0) { x => x.map(_ + 1) }
  }
