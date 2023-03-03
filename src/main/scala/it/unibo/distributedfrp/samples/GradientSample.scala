package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulation.{AggregateProgramSimulator, Environment}
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
      mux(src) {
        constant(0.0)
      } {
        lift2(nbrRange, nbr(distance))(_ + _).withoutSelf.min
      }
    }

  simulator.run {
    branch(obstacle) {
      constant(-1.0)
    } {
      gradient(source)
    }
  }

