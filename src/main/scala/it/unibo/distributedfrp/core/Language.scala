package it.unibo.distributedfrp.core

trait Language:
  self: Core =>

  def mid: Flow[DeviceId]
  def sensor[A](id: LocalSensorId): Flow[A]
  def branch[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A]
  def loop[A](init: A)(f: Flow[A] => Flow[A]): Flow[A]
  def nbr[A](a: Flow[A]): Flow[NeighborField[A]]
  def nbrSensor[A](id: NeighborSensorId): Flow[NeighborField[A]]
