package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulated.{AggregateProgramSimulator, Environment}
import it.unibo.distributedfrp.utils.Lift.*

@main def gradientSample(): Unit =
  val environment = Environment.manhattanGrid(5, 5)
  val simulator = new AggregateProgramSimulator(
    environment,
    sources = Set(0),
    obstacles = Set(2, 7, 12))

  import simulator.SimulationIncarnation._
  import simulator.SimulationIncarnation.given

  def gradient(src: Flow[Boolean]): Flow[Double] =
    loop(Double.PositiveInfinity) { distance =>
      val distances = lift2(nbrRange, nbr(distance))(_ + _).withoutSelf.min
      lift(src, distances)(if _ then 0.0 else _)
    }

  simulator.run {
    branch(obstacle) {
      Flows.constant(-1.0)
    } {
      gradient(source)
    }
  }

