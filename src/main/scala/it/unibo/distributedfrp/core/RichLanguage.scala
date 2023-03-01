package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.utils.Lift.*
import it.unibo.distributedfrp.utils.{Bounded, LowerBounded, UpperBounded}

trait RichLanguage:
  self: Core with CoreExtensions with Language =>

  given Bounded[Int] with
    override def lowerBound: Int = Int.MinValue
    override def upperBound: Int = Int.MaxValue
    override def compare(x: Int, y: Int): Int = x.compareTo(y)

  given Bounded[Double] with
    override def lowerBound: Double = Double.NegativeInfinity
    override def upperBound: Double = Double.PositiveInfinity
    override def compare(x: Double, y: Double): Int = x.compareTo(y)

  def mux[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A] =
    lift(cond, th, el)(if _ then _ else _)

  extension[A] (flow: Flow[NeighborField[A]])
    def withoutSelf: Flow[NeighborField[A]] = lift(mid, flow)((id, field) => field.withoutNeighbor(id))

    def toSet: Flow[Set[A]] = flow.map(_.foldLeft(Set.empty)(_ + _))

  extension[A : UpperBounded] (flow: Flow[NeighborField[A]])
    def min: Flow[A] = flow.map(_.fold(summon[UpperBounded[A]].upperBound)(summon[UpperBounded[A]].min))

  extension[A : LowerBounded] (flow: Flow[NeighborField[A]])
    def max: Flow[A] = flow.map(_.fold(summon[LowerBounded[A]].lowerBound)(summon[LowerBounded[A]].max))