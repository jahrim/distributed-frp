package it.unibo.distributedfrp.core

import nz.sodium.Cell

trait Model:
  self: Core =>

  trait Flow[A]:
    def exports(path: Seq[Any])(using ctx: Context): Cell[Export[A]]

  object Flow:
    def apply[A](f: Context ?=> Seq[Any] => Cell[Export[A]]): Flow[A] = new Flow[A]:
      override def exports(path: Seq[Any])(using ctx: Context): Cell[Export[A]] = f(path)

    def fromCell[A](cell: Context ?=> Cell[A]): Flow[A] = Flow(_ => cell.map(Export.atomic(_)))

    def constant[A](f: Context ?=> A): Flow[A] = fromCell(new Cell(f))

  trait NeighborField[+A]:
    def map[B](f: A => B): NeighborField[B]
    def fold[B](init: => B)(f: (B, A) => B): B

  object NeighborField:
    def apply[A](neighborsExports: Map[DeviceId, A]): NeighborField[A] = ???
