package it.unibo.distributedfrp.core

import nz.sodium._

trait Core:
  type DeviceId
  type SensorId

  protected def flowOf[A](f: Context ?=> Seq[Any] => Cell[Export[A]]): Flow[A]
  
  trait Flow[A]:
    def exports(path: Seq[Any])(using ctx: Context): Cell[Export[A]]

  type NeighborFlow[A] = Flow[NeighborField[A]]

  trait NeighborInfo:
    def sensor[A](id: SensorId): Option[A]
    def exported: Export[Any]

  trait Context:
    def selfId: DeviceId
    def sensor[A](id: SensorId): Option[Cell[A]]
    def neighbors: Cell[NeighborField[NeighborInfo]]

  case class NeighborField[A](neighborValues: Map[DeviceId, A])

