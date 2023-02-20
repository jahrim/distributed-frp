package it.unibo.distributedfrp.core

import nz.sodium._

trait Core:
  type DeviceId
  type LocalSensorId
  type NeighborSensorId
  type Context
  
  type Path = Seq[Slot]
  
  trait Flow[A]:
    def exports(path: Path)(using ctx: Context): Cell[Export[A]]

  protected def flowOf[A](f: Context ?=> Path => Cell[Export[A]]): Flow[A]

  case class NeighborField[A](neighborValues: Map[DeviceId, A]):
    override def toString: String =
      s"(${neighborValues.mkString(", ")})"

  object NeighborField:
    def empty[A]: NeighborField[A] = NeighborField(Map.empty)

    def apply[A](pairs: (DeviceId, A)*): NeighborField[A] = NeighborField(pairs.toMap)

