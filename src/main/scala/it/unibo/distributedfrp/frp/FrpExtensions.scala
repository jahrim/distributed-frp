package it.unibo.distributedfrp.frp

import nz.sodium.Cell

object FrpExtensions:
  extension[A] (cell: Cell[A])
    def flatMap[B](f: A => Cell[B]): Cell[B] = Cell.switchC(cell.map(f(_)))
