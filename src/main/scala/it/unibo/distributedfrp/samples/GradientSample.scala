package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulation.{Environment, SimulationIncarnation, Simulator}
import it.unibo.distributedfrp.utils.Liftable
import it.unibo.distributedfrp.utils.Liftable.*

@main def gradientSample(): Unit =
  val environment = Environment.manhattanGrid(5, 5)
  val incarnation = SimulationIncarnation(
    environment,
    sources = Set(0),
    obstacles = Set(2, 7, 12)
  )
  val simulator = Simulator(incarnation)

  import simulator.incarnation._
  import simulator.incarnation.given

  def gradient(src: Flow[Boolean]): Flow[Double] =
    loop(Double.PositiveInfinity) { distance =>
      mux(src) {
        constant(0.0)
      } {
        liftTwice(nbrRange, nbr(distance))(_ + _).withoutSelf.min
      }
    }

  simulator.run {
    branch(obstacle) {
      constant(-1.0)
    } {
      gradient(source)
    }
  }

