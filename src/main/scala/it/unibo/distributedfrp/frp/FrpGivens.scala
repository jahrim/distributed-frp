package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.utils.Lift
import nz.sodium.Cell

object FrpGivens:
  given Lift[Cell] with
    override def lift[A, B, C](a: Cell[A], b: Cell[B])(f: (A, B) => C): Cell[C] =
      a.lift(b, (aa, bb) => f(aa, bb))

    override def lift[A, B, C, D](a: Cell[A], b: Cell[B], c: Cell[C])(f: (A, B, C) => D): Cell[D] =
      a.lift(b, c, (aa, bb, cc) => f(aa, bb, cc))

