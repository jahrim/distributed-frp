package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.core.{Core, CoreExtensions, Export, Language}
import it.unibo.distributedfrp.frp.FrpGivens.given
import it.unibo.distributedfrp.utils.Lift
import it.unibo.distributedfrp.utils.Lift.*
import nz.sodium.{Cell, CellLoop}

trait Semantics extends Core, Language, CoreExtensions:
  override def flowOf[A](f: Context ?=> Seq[Any] => Cell[Export[A]]): Flow[A] = new Flow[A]:
    override def exports(path: Seq[Any])(using Context): Cell[Export[A]] = f(path)

  private def alignWithNeighbors[T](path: Seq[Any])(f: (Export[Any], NeighborInfo) => T)(using ctx: Context): Cell[NeighborField[T]] =
    ctx.neighbors.map(_.filterMap(field => field.exported.followPath(path).map(x => f(x, field))))

  override def mid: Flow[DeviceId] = Flows.constant(summon[Context].selfId)

  override def nbr[A](a: Flow[A]): Flow[NeighborField[A]] =
    flowOf { path =>
      val neighboringValues = alignWithNeighbors(path)((e, _) => e.root.asInstanceOf[A])
      lift(a.exports(path :+ ()), neighboringValues){ (x, n) =>
        val neighborField = NeighborField(n.neighborValues + (summon[Context].selfId -> x.root))
        Export(neighborField, Map(() -> x))
      }
    }

  override def branch[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A] =
    flowOf { path =>
      val condExport = cond.exports(path :+ ())
      val thenExport = th.exports(path :+ true)
      val elseExport = el.exports(path :+ false)
      condExport.lift(thenExport, elseExport, (c, t, e) => {
        val selected = if c.root then t else e
        Export(selected.root, Map(() -> c, c.root -> selected))
      })
    }

  override def loop[A](f: Flow[A] => Flow[A]): Flow[A] =
    flowOf { path =>
      val cellLoop = new CellLoop[Export[A]]()
      val cell = f(flowOf(_ => cellLoop)).exports(path)
      cellLoop.loop(cell)
      cell
    }

  override def nbrSensor[A](id: SensorId): Flow[NeighborField[A]] =
    flowOf { path =>
      val alignedNeighbors = alignWithNeighbors(path) { (_, n) =>
        n.sensor[A](id) match
          case Some(v) => v
          case _ => throw new IllegalArgumentException(s"Neighboring sensor with ID $id is not available")
      }
      alignedNeighbors.map(Export.atomic(_))
    }

  override def sensor[A](id: SensorId): Flow[A] =
    Flows.fromCell {
      summon[Context].sensor[A](id) match
        case Some(sensor) => sensor
        case _ => throw new IllegalArgumentException(s"Sensor with ID $id is not available")
    }
