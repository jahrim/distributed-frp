package it.unibo.distributedfrp.core

import nz.sodium.{Cell, CellLoop}
import it.unibo.distributedfrp.utils.Lift._
import it.unibo.distributedfrp.frp.FrpGivens.given

trait Semantics extends Core with Model with Language:
  private val CONDITION_KEY = 0
  private val THEN_KEY = 1
  private val ELSE_KEY = 2
  private val NBR_KEY = 0

  private def alignWithNeighbors[T](path: Seq[Any])(mapper: (Export[Any], NeighborInfo) => T)(using ctx: Context): Cell[Map[DeviceId, T]] =
    ctx.neighbors.map { nbrs =>
      nbrs
        .flatMap { case (d, n) => n.exported.followPath(path).map(x => (d, (x, n))) }
        .map { case (d, (e, n)) => (d, mapper(e, n)) }
    }

  override def mid: Flow[DeviceId] = Flow.constant(summon[Context].selfId)

  override def nbr[A](a: Flow[A]): Flow[NeighborField[A]] = Flow { path =>
    val neighboringValues = alignWithNeighbors(path)((e, _) => e.root.asInstanceOf[A])
    lift(a.exports(path :+ NBR_KEY), neighboringValues){ (x, n) =>
      val neighborField = NeighborField(n + (summon[Context].selfId -> x.root))
      Export.wrapper(neighborField, NBR_KEY, x)
    }
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
    val cellLoop = new CellLoop[Export[A]]()
    val cell = f(Flow(_ => cellLoop)).exports(path)
    cellLoop.loop(cell)
    cell
  }

  override def nbrSensor[A](id: SensorId): Flow[NeighborField[A]] = Flow { path =>
    val alignedNeighbors = alignWithNeighbors(path) { (_, n) =>
      n.sensor[A](id) match
        case Some(v) => v
        case _ => throw new IllegalArgumentException(s"Neighboring sensor with ID $id is not available")
    }
    alignedNeighbors.map(n => Export.atomic(NeighborField(n)))
  }

  override def sensor[A](id: SensorId): Flow[A] = Flow.fromCell {
    summon[Context].sensor[A](id) match
      case Some(sensor) => sensor
      case _ => throw new IllegalArgumentException(s"Sensor with ID $id is not available")
  }
