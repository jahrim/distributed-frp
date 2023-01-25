package it.unibo.distributedfrp.core

import nz.sodium.{Cell, CellLoop}
import it.unibo.distributedfrp.utils.Lift.*
import it.unibo.distributedfrp.frp.FrpGivens.given

trait Semantics extends Core with Language with CoreExtensions:
  private val CONDITION_KEY = 0
  private val THEN_KEY = 1
  private val ELSE_KEY = 2
  private val NBR_KEY = 0

  override type Flow[A] = ExportFlow[A]

  trait ExportFlow[A]:
    def exports(path: Seq[Any])(using ctx: Context): Cell[Export[A]]

  object ExportFlow:
    def apply[A](f: Context ?=> Seq[Any] => Cell[Export[A]]): Flow[A] = new Flow[A]:
      override def exports(path: Seq[Any])(using ctx: Context): Cell[Export[A]] = f(path)

    def fromCell[A](cell: Context ?=> Cell[A]): Flow[A] = ExportFlow(_ => cell.map(Export.atomic(_)))

    def constant[A](f: Context ?=> A): Flow[A] = fromCell(new Cell(f))

  private def alignWithNeighbors[T](path: Seq[Any])(f: (Export[Any], NeighborInfo) => T)(using ctx: Context): Cell[NeighborField[T]] =
    ctx.neighbors.map(_.filterMap(field => field.exported.followPath(path).map(x => f(x, field))))

  override def mid: Flow[DeviceId] = ExportFlow.constant(summon[Context].selfId)

  override def nbr[A](a: Flow[A]): Flow[NeighborField[A]] = ExportFlow { path =>
    val neighboringValues = alignWithNeighbors(path)((e, _) => e.root.asInstanceOf[A])
    lift(a.exports(path :+ NBR_KEY), neighboringValues){ (x, n) =>
      val neighborField = NeighborField(n.neighborValues + (summon[Context].selfId -> x.root))
      Export.wrapper(neighborField, NBR_KEY, x)
    }
  }

  override def branch[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A] = ExportFlow { path =>
    val condExport = cond.exports(path :+ ())
    val thenExport = th.exports(path :+ THEN_KEY)
    val elseExport = el.exports(path :+ ELSE_KEY)
    condExport.lift(thenExport, elseExport, (c, t, e) => {
      val selected = if c.root then t else e
      Export(selected.root, Map(() -> c, c.root -> selected))
    })
  }

  override def loop[A](f: Flow[A] => Flow[A]): Flow[A] = ExportFlow { path =>
    val cellLoop = new CellLoop[Export[A]]()
    val cell = f(ExportFlow(_ => cellLoop)).exports(path)
    cellLoop.loop(cell)
    cell
  }

  override def nbrSensor[A](id: SensorId): Flow[NeighborField[A]] = ExportFlow { path =>
    val alignedNeighbors = alignWithNeighbors(path) { (_, n) =>
      n.sensor[A](id) match
        case Some(v) => v
        case _ => throw new IllegalArgumentException(s"Neighboring sensor with ID $id is not available")
    }
    alignedNeighbors.map(Export.atomic(_))
  }

  override def sensor[A](id: SensorId): Flow[A] = ExportFlow.fromCell {
    summon[Context].sensor[A](id) match
      case Some(sensor) => sensor
      case _ => throw new IllegalArgumentException(s"Sensor with ID $id is not available")
  }
