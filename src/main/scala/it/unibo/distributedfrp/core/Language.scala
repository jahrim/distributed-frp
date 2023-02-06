package it.unibo.distributedfrp.core

trait Language:
  self: Core =>

  def nbr[A](a: Flow[A]): Flow[NeighborField[A]]
  def loop[A](f: Flow[A] => Flow[A]): Flow[A]
  def branch[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A]
  def mid: Flow[DeviceId]
  def sensor[A](id: SensorId): Flow[A]
  def nbrSensor[A](id: SensorId): Flow[NeighborField[A]]
