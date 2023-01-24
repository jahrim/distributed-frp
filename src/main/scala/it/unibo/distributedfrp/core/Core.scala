package it.unibo.distributedfrp.core

import nz.sodium._

trait Core:
  type DeviceId
  type SensorId

  trait NeighborInfo:
    def sensor[A](id: SensorId): Option[A]
    def exported: Export[Any]

  trait Context:
    def selfId: DeviceId
    def sensor[A](id: SensorId): Option[Cell[A]]
    def neighbors: Cell[Map[DeviceId, NeighborInfo]]
