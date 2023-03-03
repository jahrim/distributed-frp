package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.core.*
import it.unibo.distributedfrp.core.Slot.*
import it.unibo.distributedfrp.frp.FrpGivens.given
import it.unibo.distributedfrp.frp.FrpExtensions.*
import it.unibo.distributedfrp.utils.Lift
import it.unibo.distributedfrp.utils.Lift.*
import nz.sodium.time.SecondsTimerSystem
import nz.sodium.{Cell, CellLoop, Operational, Stream, Transaction}

trait Semantics:
  self: Core with Language with CoreExtensions =>

  override type Context <: BasicContext
  type NeighborState <: BasicNeighborState

  trait BasicNeighborState:
    def sensor[A](id: NeighborSensorId): A
    def exported: Export[Any]

  trait BasicContext:
    private val DEFAULT_LOOPING_PERIOD = 0.1
    val timerSystem: SecondsTimerSystem = new SecondsTimerSystem
    def selfId: DeviceId
    def sensor[A](id: LocalSensorId): Cell[A]
    def neighbors: Cell[Map[DeviceId, NeighborState]]
    def loopingPeriod: Double = DEFAULT_LOOPING_PERIOD

  private def context(using ctx: Context) = ctx

  override def flowOf[A](f: Context ?=> Path => Cell[Export[A]]): Flow[A] = new Flow[A]:
    override def exports(path: Path)(using Context): Cell[Export[A]] = f(path).calm

  private def alignWithNeighbors[T](path: Path)(f: (Export[Any], NeighborState) => T)(using ctx: Context): Cell[Map[DeviceId, T]] =
    def align(neighbors: Map[DeviceId, NeighborState]): Map[DeviceId, T] =
      neighbors.flatMap { (neighborId, neighborState) =>
        neighborState
          .exported
          .followPath(path)
          .map(alignedExport => (neighborId, f(alignedExport, neighborState)))
      }
    ctx.neighbors.map(align(_))

  override val mid: Flow[DeviceId] = Flows.constant(context.selfId)

  override def constant[A](a: A): Flow[A] = Flows.constant(a)

  override def nbr[A](a: Flow[A]): Flow[NeighborField[A]] =
    flowOf { path =>
      val neighboringValues = alignWithNeighbors(path :+ Nbr)((e, _) => e.root.asInstanceOf[A])
      lift(a.exports(path :+ Nbr), neighboringValues){ (x, n) =>
        val neighborField = NeighborField(n + (context.selfId -> x.root))
        Export(neighborField, Nbr -> x)
      }
    }

  private def conditional[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A])(combine: (Export[Boolean], Export[A], Export[A]) => Export[A]): Flow[A] =
    flowOf { path =>
      val condExport = cond.exports(path :+ Condition)
      val thenExport = th.exports(path :+ Then)
      val elseExport = el.exports(path :+ Else)
      lift(condExport, thenExport, elseExport)(combine)
    }

  override def branch[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A] =
    conditional(cond)(th)(el) { (c, t, e) =>
      val selected = if c.root then t else e
      val selectedSlot = if c.root then Then else Else
      Export(selected.root, Condition -> c, selectedSlot -> selected)
    }

  def mux[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A] =
    conditional(cond)(th)(el) { (c, t, e) =>
      Export(if c.root then t.root else e.root, Condition -> c, Then -> t, Else -> e)
    }

  override def loop[A](init: A)(f: Flow[A] => Flow[A]): Flow[A] =
    flowOf { path =>
      val prev = context.neighbors
        .map(n => n
          .get(context.selfId)
          .flatMap(_.exported.followPath(path))
          .map(e => Export(e.root.asInstanceOf[A]))
          .getOrElse(Export(init))
        )
      f(flowOf(_ => prev)).exports(path)
    }

  override def nbrSensor[A](id: NeighborSensorId): Flow[NeighborField[A]] =
    flowOf { path =>
      val alignedNeighbors = alignWithNeighbors(path)((_, n) => n.sensor[A](id))
      alignedNeighbors.map(x => Export(NeighborField(x)))
    }

  override def sensor[A](id: LocalSensorId): Flow[A] =
    Flows.fromCell(summon[Context].sensor[A](id))
