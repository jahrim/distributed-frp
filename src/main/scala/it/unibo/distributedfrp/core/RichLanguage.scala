package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.core.Slot.Operand
import it.unibo.distributedfrp.utils.{Bounded, Liftable, LowerBounded, UpperBounded}

trait RichLanguage extends Language:
  self: Core =>

  given Bounded[Int] with
    override def lowerBound: Int = Int.MinValue
    override def upperBound: Int = Int.MaxValue
    override def compare(x: Int, y: Int): Int = x.compareTo(y)

  given Bounded[Double] with
    override def lowerBound: Double = Double.NegativeInfinity
    override def upperBound: Double = Double.PositiveInfinity
    override def compare(x: Double, y: Double): Int = x.compareTo(y)

  extension[A] (flow: Flow[NeighborField[A]])
    def withoutSelf: Flow[NeighborField[A]] = lift(mid, flow)((id, field) => field.withoutNeighbor(id))

    def toSet: Flow[Set[A]] = flow.map(_.foldLeft(Set.empty)(_ + _))

  extension[A : UpperBounded] (flow: Flow[NeighborField[A]])
    def min: Flow[A] = flow.map(_.foldLeft(summon[UpperBounded[A]].upperBound)(summon[UpperBounded[A]].min))

  extension[A : LowerBounded] (flow: Flow[NeighborField[A]])
    def max: Flow[A] = flow.map(_.foldLeft(summon[LowerBounded[A]].lowerBound)(summon[LowerBounded[A]].max))

  def liftEach[A, B, C](a: Flow[NeighborField[A]], b: Flow[NeighborField[B]])(f: (A, B) => C): Flow[NeighborField[C]] =
    lift(a, b)((aa, bb) => lift(aa, bb)(f))

  def liftEach[A, B, C, D](a: Flow[NeighborField[A]], b: Flow[NeighborField[B]], c: Flow[NeighborField[C]])(f: (A, B, C) => D): Flow[NeighborField[D]] =
    lift(a, b, c)((aa, bb, cc) => lift(aa, bb, cc)(f))