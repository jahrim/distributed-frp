package it.unibo.distributedfrp.core

import nz.sodium._

trait Core:
  type DeviceId
  type SensorId

  type Flow[_]

  trait NeighborInfo:
    def sensor[A](id: SensorId): Option[A]
    def exported: Export[Any]

  trait Context:
    def selfId: DeviceId
    def sensor[A](id: SensorId): Option[Cell[A]]
    def neighbors: Cell[NeighborField[NeighborInfo]]

  case class NeighborField[A](neighborValues: Map[DeviceId, A])

