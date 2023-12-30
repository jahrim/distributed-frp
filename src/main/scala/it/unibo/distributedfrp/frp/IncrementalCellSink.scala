package it.unibo.distributedfrp.frp

import nz.sodium.{Cell, CellSink}

class IncrementalCellSink[A](initValue: A, calm: Boolean = false):
  private var currentValue: A = initValue
  private val cellSink: CellSink[A] = new CellSink(initValue)

  def cell: Cell[A] = this.cellSink

  def set(a: A): Unit = this.update(_ => a)

  def update(f: A => A): Unit =
    synchronized {
      val old = this.currentValue
      this.currentValue = f(this.currentValue)
      if !calm || old != this.currentValue then this.cellSink.send(this.currentValue)
    }
