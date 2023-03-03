package it.unibo.distributedfrp.core

import nz.sodium.Cell
import nz.sodium.time.SecondsTimerSystem
import it.unibo.distributedfrp.frp.FrpExtensions._

trait SemanticsModel:
  self: Core with Language =>

  override type NeighborField[+A] = Map[DeviceId, A]
  override type Context <: BasicContext
  type NeighborState <: BasicNeighborState
  override type Export[+A] = ExportTree[A]
  override type Path = Seq[Slot]

  trait BasicNeighborState:
    def sensor[A](id: NeighborSensorId): A
    def exported: Export[Any]

  trait BasicContext:
    private val DEFAULT_LOOPING_PERIOD = 0.1
    val timerSystem: SecondsTimerSystem = new SecondsTimerSystem
    def selfId: DeviceId
    def sensor[A](id: LocalSensorId): Cell[A]
    def neighbors: Cell[Map[DeviceId, NeighborState]]
    def loopingPeriod: Double = DEFAULT_LOOPING_PERIOD

  extension[A] (field: NeighborField[A])
    def map[B](f: A => B): NeighborField[B] = field.map((d, x) => (d, f(x)))

    def withNeighbor(neighborId: DeviceId, value: A): NeighborField[A] =
      field + (neighborId -> value)

    def withoutNeighbor(neighborId: DeviceId): NeighborField[A] =
      field - neighborId

    def foldLeft[R](seed: R)(combine: (R, A) => R): R =
      field.values.foldLeft(seed)(combine)

  object Flows:
    def of[A](f: Context ?=> Path => Cell[Export[A]]): Flow[A] = new Flow[A]:
      override def run(path: Path)(using Context): Cell[Export[A]] = f(path)

    def fromCell[A](cell: Context ?=> Cell[A]): Flow[A] = of(_ => cell.map(ExportTree(_)))
    def constant[A](a: Context ?=> A): Flow[A] = fromCell(new Cell(a))

  override def lift[A, B, C](a: NeighborField[A], b: NeighborField[B])(f: (A, B) => C): NeighborField[C] =
    val commonDevices = a.keySet intersect b.keySet
    commonDevices.map(x => (x, f(a(x), b(x)))).toMap

  override def lift[A, B, C, D](a: NeighborField[A], b: NeighborField[B], c: NeighborField[C])(f: (A, B, C) => D): NeighborField[D] =
    val commonDevices = a.keySet intersect b.keySet intersect c.keySet
    commonDevices.map(x => (x, f(a(x), b(x), c(x)))).toMap
