package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.core.*
import it.unibo.distributedfrp.core.Slot.*
import it.unibo.distributedfrp.frp.FrpExtensions.*
import it.unibo.distributedfrp.frp.FrpExtensions.given
import it.unibo.distributedfrp.utils.Liftable
import nz.sodium.time.SecondsTimerSystem
import nz.sodium.{Cell, CellLoop, Operational, Stream, Transaction}

trait ConstructsSemantics extends Language:
  self: Core with SemanticsModel =>

  private def ctx(using Context): Context = summon[Context]

  private def alignWithNeighbors[T](path: Path)(f: (Export[Any], NeighborState) => T)(using ctx: Context): Cell[Map[DeviceId, T]] =
    def align(neighbors: Map[DeviceId, NeighborState]): Map[DeviceId, T] =
      neighbors.flatMap { (neighborId, neighborState) =>
        neighborState
          .exported
          .followPath(path)
          .map(alignedExport => (neighborId, f(alignedExport, neighborState)))
      }
    ctx.neighbors.map(align(_))

  override val mid: Flow[DeviceId] = Flows.constant(ctx.selfId)

  override def constant[A](a: A): Flow[A] = Flows.constant(a)

  override def sensor[A](id: LocalSensorId): Flow[A] = Flows.fromCell(ctx.sensor[A](id))

  override def nbr[A](a: Flow[A]): Flow[NeighborField[A]] =
    Flows.of { path =>
      val neighboringValues = alignWithNeighbors(path :+ Nbr)((e, _) => e.root.asInstanceOf[A])
      Liftable.lift(a.run(path :+ Nbr), neighboringValues){ (x, n) =>
        val neighborField = n + (ctx.selfId -> x.root)
        ExportTree(neighborField, Nbr -> x)
      }
    }

  override def nbrSensor[A](id: NeighborSensorId): Flow[NeighborField[A]] =
    Flows.of { path =>
      alignWithNeighbors(path)((_, n) => n.sensor[A](id)).map(ExportTree(_))
    }

  private def conditional[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A])(combine: (Export[Boolean], Export[A], Export[A]) => Export[A]): Flow[A] =
    Flows.of { path =>
      val condExport = cond.run(path :+ Condition)
      val thenExport = th.run(path :+ Then)
      val elseExport = el.run(path :+ Else)
      Liftable.lift(condExport, thenExport, elseExport)(combine)
    }

  override def branch[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A] =
    conditional(cond)(th)(el) { (c, t, e) =>
      val selected = if c.root then t else e
      val selectedSlot = if c.root then Then else Else
      ExportTree(selected.root, Condition -> c, selectedSlot -> selected)
    }

  def mux[A](cond: Flow[Boolean])(th: Flow[A])(el: Flow[A]): Flow[A] =
    conditional(cond)(th)(el) { (c, t, e) =>
      ExportTree(if c.root then t.root else e.root, Condition -> c, Then -> t, Else -> e)
    }

  override def loop[A](init: A)(f: Flow[A] => Flow[A]): Flow[A] =
    Flows.of { path =>
      val prev = ctx
        .neighbors
        .map(nbrs => {
          nbrs
            .get(ctx.selfId)
            .flatMap(_.exported.followPath(path))
            .map(e => ExportTree(e.root.asInstanceOf[A]))
            .getOrElse(ExportTree(init))
        })
      f(Flows.of(_ => prev)).run(path)
    }

  extension[A] (flow: Flow[A])
    def map[B](f: A => B): Flow[B] =
      Flows.of { path =>
        flow.run(path :+ Operand(0)).map(e => ExportTree(f(e.root), Operand(0) -> e))
      }

  override def lift[A, B, C](a: Flow[A], b: Flow[B])(f: (A, B) => C): Flow[C] =
    Flows.of { path =>
      Liftable.lift(
        a.run(path :+ Operand(0)),
        b.run(path :+ Operand(1))
      )(
        (aa, bb) => ExportTree(
          f(aa.root, bb.root),
          Operand(0) -> aa,
          Operand(1) -> bb)
      )
    }

  override def lift[A, B, C, D](a: Flow[A], b: Flow[B], c: Flow[C])(f: (A, B, C) => D): Flow[D] =
    Flows.of { path =>
      Liftable.lift(
        a.run(path :+ Operand(0)),
        b.run(path :+ Operand(1)),
        c.run(path :+ Operand(2))
      )(
        (aa, bb, cc) => ExportTree(
          f(aa.root, bb.root, cc.root),
          Operand(0) -> aa,
          Operand(1) -> bb,
          Operand(2) -> cc)
      )
    }