package it.unibo.distributedfrp.core

import nz.sodium._

trait Core:
  type DeviceId
  type SensorId
  type Context
  
  type Path = Seq[Any]
  
  trait Flow[A]:
    def exports(path: Seq[Any])(using ctx: Context): Cell[Export[A]]

  protected def flowOf[A](f: Context ?=> Path => Cell[Export[A]]): Flow[A]

  case class NeighborField[A](neighborValues: Map[DeviceId, A])

  object NeighborField:
    def empty[A]: NeighborField[A] = NeighborField(Map.empty)

    def apply[A](pairs: (DeviceId, A)*): NeighborField[A] = NeighborField(pairs.toMap)

