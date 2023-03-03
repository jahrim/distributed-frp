package it.unibo.distributedfrp.core

trait Language:
  self: Core =>

  type DeviceId
  type NeighborField[+_]
  type LocalSensorId
  type NeighborSensorId

  def mid: Flow[DeviceId]
  def constant[A](a: A): Flow[A]
  def sensor[A](id: LocalSensorId): Flow[A]
  def branch[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A]
  def mux[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A]
  def loop[A](init: A)(f: Flow[A] => Flow[A]): Flow[A]
  def nbr[A](a: Flow[A]): Flow[NeighborField[A]]
  def nbrSensor[A](id: NeighborSensorId): Flow[NeighborField[A]]
  def lift[A, B, C](a: Flow[A], b: Flow[B])(f: (A, B) => C): Flow[C]
  def lift[A, B, C, D](a: Flow[A], b: Flow[B], c: Flow[C])(f: (A, B, C) => D): Flow[D]
  def lift[A, B, C](a: NeighborField[A], b: NeighborField[B])(f: (A, B) => C): NeighborField[C]
  def lift[A, B, C, D](a: NeighborField[A], b: NeighborField[B], c: NeighborField[C])(f: (A, B, C) => D): NeighborField[D]

  extension[A] (flow: Flow[A])
    def map[B](f: A => B): Flow[B]

  extension[A] (field: NeighborField[A])
    def foldLeft[T](seed: T)(combine: (T, A) => T): T
    def map[B](f: A => B): NeighborField[B]
    def withNeighbor(id: DeviceId, value: A): NeighborField[A]
    def withoutNeighbor(id: DeviceId): NeighborField[A]
