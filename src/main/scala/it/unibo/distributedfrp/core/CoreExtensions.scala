package it.unibo.distributedfrp.core

trait CoreExtensions:
  self: Core =>

  extension [A](field: NeighborField[A])
    def map[B](f: A => B): NeighborField[B] =
      NeighborField(field.neighborValues.map((d, x) => (d, f(x))))

    def filterMap[B](f: A => Option[B]): NeighborField[B] =
      NeighborField(field.neighborValues.flatMap((d, x) => f(x).map((d, _))))
