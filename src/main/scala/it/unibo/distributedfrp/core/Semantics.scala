package it.unibo.distributedfrp.core

import nz.sodium.Cell
import it.unibo.distributedfrp.frp.CellExtensions._

trait Semantics extends Core with Model with Language:
  extension (ctx: Context)
    private def alignedNeighbors(path: Seq[Any]): Cell[Map[DeviceId, Export[Any]]] =
      ctx.neighbors.map(_.flatMap { case (d, n) => n.exported.followPath(path).map((d, _)) })

  val CONDITION_KEY = 0
  val THEN_KEY = 1
  val ELSE_KEY = 2
  val NBR_KEY = 0

  override def mid: Flow[DeviceId] = Flow.constant(summon[Context].selfId)

  override def nbr[A](a: Flow[A]): Flow[NeighborField[A]] = Flow { path =>
    val alignedNeighbors = summon[Context].alignedNeighbors(path)
    val selfExport = a.exports(path :+ NBR_KEY)
    selfExport.lift(alignedNeighbors, (s, n) => Export.wrapper(NeighborField(s, n), NBR_KEY, s))
  }

  override def branch[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A] = Flow { path =>
    val condExport = cond.exports(path :+ CONDITION_KEY)
    val thenExport = th.exports(path :+ THEN_KEY)
    val elseExport = el.exports(path :+ ELSE_KEY)
    condExport.lift(thenExport, elseExport, (c, t, e) => {
      val selected = if c.root then t else e
      val index = if c.root then THEN_KEY else ELSE_KEY
      Export(selected.root, Map(0 -> c, index -> selected))
    })
  }

  override def loop[A](init: => A)(f: Flow[A] => Flow[A]): Flow[A] = Flow { path =>
    ???
  }

  override def nbrSensor[A](name: SensorId): Flow[NeighborField[A]] = ???

  override def sensor[A](id: SensorId): Flow[A] = ???