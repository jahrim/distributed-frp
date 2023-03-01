package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.utils.{Bounded, Lift, LowerBounded, UpperBounded}
import Lift.*
import nz.sodium.Cell
import it.unibo.distributedfrp.frp.FrpGivens.given
import it.unibo.distributedfrp.core.Slot._

trait CoreExtensions:
  self: Core =>

  object Flows:
    def fromCell[A](cell: Context ?=> Cell[A]): Flow[A] = flowOf(_ => cell.map(Export(_)))

    def constant[A](a: Context ?=> A): Flow[A] = fromCell(new Cell(a))

  extension[A] (flow: Flow[A])
    def map[B](f: A => B): Flow[B] = flowOf(path => flow.exports(path :+ LiftOperand(0)).map(e => Export(f(e.root), LiftOperand(0) -> e)))

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

    def foldLeft[R](seed: R)(combine: (R, A) => R): R =
      field.neighborValues.values.foldLeft(seed)(combine)

    def fold(seed: A)(combine: (A, A) => A): A = foldLeft(seed)(combine)

  given Lift[NeighborField] with
    override def lift[A, B, C](a: NeighborField[A], b: NeighborField[B])(f: (A, B) => C): NeighborField[C] = {
      val commonDevices = a.neighborValues.keySet intersect b.neighborValues.keySet
      NeighborField(commonDevices.map(d => (d, f(a.neighborValues(d), b.neighborValues(d)))).toMap)
    }

  given Lift[Flow] with
    override def lift[A, B, C](a: Flow[A], b: Flow[B])(f: (A, B) => C): Flow[C] =
      flowOf { path =>
        Lift.lift(a.exports(path :+ LiftOperand(0)), b.exports(path :+ LiftOperand(1)))((aa, bb) => Export(f(aa.root, bb.root), LiftOperand(0) -> aa, LiftOperand(1) -> bb))
      }
