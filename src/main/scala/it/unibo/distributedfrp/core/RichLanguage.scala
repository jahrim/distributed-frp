package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.utils.Lift.*
import it.unibo.distributedfrp.utils.{LowerBounded, UpperBounded}

trait RichLanguage:
  self: Core with CoreExtensions with Language =>

  extension[A] (flow: Flow[NeighborField[A]])
    def withoutSelf: Flow[NeighborField[A]] = lift(mid, flow)((id, field) => field.withoutNeighbor(id))

    def toSet: Flow[Set[A]] = flow.map(_.foldLeft(Set.empty)(_ + _))

  extension[A : UpperBounded] (flow: Flow[NeighborField[A]])
    def min: Flow[A] = flow.map(_.fold(summon[UpperBounded[A]].upperBound)(summon[UpperBounded[A]].min))

  extension[A : LowerBounded] (flow: Flow[NeighborField[A]])
    def max: Flow[A] = flow.map(_.fold(summon[LowerBounded[A]].lowerBound)(summon[LowerBounded[A]].max))