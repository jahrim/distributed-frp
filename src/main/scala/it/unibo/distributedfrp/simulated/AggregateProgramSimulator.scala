package it.unibo.distributedfrp.simulated

import it.unibo.distributedfrp.core.Export
import it.unibo.distributedfrp.frp.IncrementalCellSink
import it.unibo.distributedfrp.incarnation.Incarnation
import nz.sodium.{Cell, CellSink, Transaction}

import java.util.concurrent.{ExecutorService, Executors}
import scala.math.*

class AggregateProgramSimulator(environment: Environment, executor: ExecutorService = Executors.newSingleThreadExecutor):
  object SimulationIncarnation extends Incarnation:
    override type DeviceId = Int
    override type SensorId = String
    override type Context = SimulationContext
    override type NeighborInfo = SimulationNeighborInfo

    override def context(selfId: DeviceId): Context = new SimulationContext(selfId)

    class SimulationContext(val selfId: DeviceId) extends BasicContext:
      private val neighborsSink = new IncrementalCellSink[NeighborField[NeighborInfo]](NeighborField(), calm = true)

      def neighborExported(neighborId: DeviceId, exported: Export[Any]): Unit =
        neighborsSink.update(_.withNeighbor(neighborId)(SimulationNeighborInfo(selfId, neighborId, exported)))

      override def neighbors: Cell[NeighborField[NeighborInfo]] = neighborsSink.cell

      override def sensor[A](id: SensorId): Cell[A] = id match
        case "SENSOR_1" => new Cell((selfId == 0).asInstanceOf[A])

    case class SimulationNeighborInfo(selfId: DeviceId, neighborId: DeviceId, exported: Export[Any]) extends BasicNeighborInfo:
      override def sensor[A](id: SensorId): A = id match
        case "NBR_RANGE" => (environment.position(selfId), environment.position(neighborId)) match
          case ((x1, y1), (x2, y2)) => hypot(x1 - x2, y1 - y2).asInstanceOf[A]

  import SimulationIncarnation._

  def run[A](flow: Flow[A]): Unit =
    val contexts = for (i <- 0 until environment.nDevices) yield context(i)
    val exports = Transaction.run(() => contexts.map(ctx => (ctx.selfId, flow.exports(Seq.empty)(using ctx))))
    exports.foreach((id, exp) => exp.listen(e => {
      println(s"Device $id exported:\n$e")
      executor.execute(() => deviceExported(id, e, contexts))
    }))

  private def deviceExported[A](id: DeviceId, exported: Export[A], contexts: Seq[Context]): Unit =
    environment.neighbors(id).foreach { n =>
      contexts(n).neighborExported(id, exported)
    }
