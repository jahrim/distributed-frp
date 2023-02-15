package it.unibo.distributedfrp.frp

import nz.sodium.{Cell, CellSink}

class IncrementalCellSink[A](initValue: A, calm: Boolean = false):
  private var currentValue: A = initValue
  private val cellSink: CellSink[A] = new CellSink(initValue)

  def cell: Cell[A] = cellSink

  def update(f: A => A): Unit =
    val old = currentValue
    currentValue = f(currentValue)
    if !calm || !old.equals(currentValue) then cellSink.send(currentValue)

