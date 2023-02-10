package it.unibo.distributedfrp.programs

import it.unibo.distributedfrp.incarnation.Incarnation
import nz.sodium.StreamSink

object TestIncarnation extends Incarnation:
  override type DeviceId = Int
  override type SensorId = String

  override def context(selfId: Int): Context = ???

import TestIncarnation._
import TestIncarnation.given
import it.unibo.distributedfrp.utils.Lift._

class Gradient extends Program[Double]:
  override def main: Flow[Double] = gradient(sensor("source"))

  private def gradient(src: Flow[Boolean]): Flow[Double] =
    loop(new StreamSink())(Double.PositiveInfinity) { distance =>
      for {
        s <- src
        r <- nbrSensor[Double]("NBR_RANGE")
        d <- nbr(distance)
      } yield if s then 0.0 else lift(r, d)(_ + _).min
    }
