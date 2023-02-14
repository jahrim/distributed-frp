package it.unibo.distributedfrp.frp

import nz.sodium.{Cell, CellSink}

class IncrementalCellSink[A](initValue: A):
  private var currentValue: A = initValue
  private val cellSink: CellSink[A] = new CellSink(initValue)

  def cell: Cell[A] = cellSink

  def update(f: A => A): Unit =
    currentValue = f(currentValue)
    cellSink.send(currentValue)

