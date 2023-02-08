package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.utils.{Bounded, Lift, LowerBounded, UpperBounded}
import Lift.*
import nz.sodium.Cell
import it.unibo.distributedfrp.frp.FrpGivens.given

trait CoreExtensions:
  self: Core =>

  object Flows:
    def fromCell[A](cell: Context ?=> Cell[A]): Flow[A] = flowOf(_ => cell.map(Export(_)))

    def constant[A](a: Context ?=> A): Flow[A] = fromCell(new Cell(a))

  extension[A] (flow: Flow[A])
    def map[B](f: A => B): Flow[B] = flowOf(path => flow.exports(path :+ ()).map(e => Export(f(e.root), () -> e)))

    def flatMap[B](f: A => Flow[B]): Flow[B] = ???

  extension[A] (field: NeighborField[A])
    def update[B](f: Map[DeviceId, A] => Map[DeviceId, B]): NeighborField[B] =
      NeighborField(f(field.neighborValues))

    def map[B](f: A => B): NeighborField[B] =
      field.update(_.map((d, x) => (d, f(x))))

    def get(neighborId: DeviceId): Option[A] = field.neighborValues.get(neighborId)

    def withNeighbor(neighborId: DeviceId)(value: A): NeighborField[A] =
      field.update(_ + (neighborId -> value))

    def updateNeighbor(neighborId: DeviceId)(f: A => A): NeighborField[A] =
      val toBeAdded = field.get(neighborId) match
        case Some(v) => f(v)
        case _ => throw new IllegalStateException(s"Neighbor with ID $neighborId does not exist in the current neighbor field")
      field.update(_ + (neighborId -> toBeAdded))

    def withoutNeighbor(neighborId: DeviceId): NeighborField[A] =
      field.update(_ - neighborId)

    def filterMap[B](f: A => Option[B]): NeighborField[B] =
      field.update(_.flatMap((d, x) => f(x).map((d, _))))

    def fold(seed: A)(combine: (A, A) => A): A =
      field.neighborValues.values.fold(seed)(combine)

  extension[A : UpperBounded] (field: NeighborField[A])
    def min: A = field.fold(summon[UpperBounded[A]].upperBound)(summon[UpperBounded[A]].min)

  extension[A : LowerBounded] (field: NeighborField[A] )
    def max: A = field.fold(summon[LowerBounded[A]].lowerBound)(summon[LowerBounded[A]].max)

  given Lift[NeighborField] with
    override def lift[A, B, C](a: NeighborField[A], b: NeighborField[B])(f: (A, B) => C): NeighborField[C] = {
      val commonDevices = a.neighborValues.keySet intersect b.neighborValues.keySet
      NeighborField(commonDevices.map(d => (d, f(a.neighborValues(d), b.neighborValues(d)))).toMap)
    }

  given Lift[Flow] with
    override def lift[A, B, C](a: Flow[A], b: Flow[B])(f: (A, B) => C): Flow[C] =
      flowOf { path =>
        Lift.lift(a.exports(path :+ 0), b.exports(path :+ 1))((aa, bb) => Export(f(aa.root, bb.root), 0 -> aa, 1 -> bb))
      }

  given Bounded[Int] with
    override def lowerBound: Int = Int.MinValue
    override def upperBound: Int = Int.MaxValue
    override def compare(x: Int, y: Int): Int = x.compareTo(y)

  given Bounded[Double] with
    override def lowerBound: Double = Double.PositiveInfinity
    override def upperBound: Double = Double.NegativeInfinity
    override def compare(x: Double, y: Double): Int = x.compareTo(y)
