package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulated.{AggregateProgramSimulator, Environment}
import it.unibo.distributedfrp.utils.Lift._
import nz.sodium.StreamSink

@main def gradientSample(): Unit =
  val environment = Environment.grid(2, 2)
  val simulator = new AggregateProgramSimulator(environment)
  val trigger = new StreamSink[Unit]

  import simulator.SimulationIncarnation._
  import simulator.SimulationIncarnation.given

  def gradient(src: Flow[Boolean]): Flow[Double] =
    loop(trigger)(Double.PositiveInfinity) { distance =>
      val distances = lift(nbrSensor[Double]("NBR_RANGE"), nbr(distance))(lift(_, _)(_ + _).min)
      lift(src, distances)(if _ then 0.0 else _)
    }

  simulator.run(gradient(sensor("SENSOR_1")))

