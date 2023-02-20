package it.unibo.distributedfrp.simulated

import it.unibo.distributedfrp.core.{Export, Incarnation}
import it.unibo.distributedfrp.frp.IncrementalCellSink
import nz.sodium.{Cell, CellSink, Transaction}

import java.util.concurrent.{ExecutorService, Executors}
import scala.math.*

enum SimulationLocalSensor:
  case Source
  case Obstacle

enum SimulationNeighborSensor:
  case NbrRange

import SimulationLocalSensor._
import SimulationNeighborSensor._

class AggregateProgramSimulator(
                                 environment: Environment,
                                 sources: Set[Int] = Set.empty,
                                 obstacles: Set[Int] = Set.empty,
                                 executor: ExecutorService = Executors.newSingleThreadExecutor):
  object SimulationIncarnation extends Incarnation:
    override type DeviceId = Int
    override type LocalSensorId = SimulationLocalSensor
    override type NeighborSensorId = SimulationNeighborSensor
    override type Context = SimulationContext
    override type NeighborInfo = SimulationNeighborInfo

    override def context(selfId: DeviceId): Context = new SimulationContext(selfId)

    def source: Flow[Boolean] = sensor[Boolean](Source)
    def obstacle: Flow[Boolean] = sensor[Boolean](Obstacle)
    def nbrRange: Flow[NeighborField[Double]] = nbrSensor[Double](NbrRange)

    class SimulationContext(val selfId: DeviceId) extends BasicContext:
      private val neighborsSink = new IncrementalCellSink[NeighborField[NeighborInfo]](NeighborField(), calm = true)

      def neighborExported(neighborId: DeviceId, exported: Export[Any]): Unit =
        neighborsSink.update(_.withNeighbor(neighborId)(SimulationNeighborInfo(selfId, neighborId, exported)))

      override def neighbors: Cell[NeighborField[NeighborInfo]] = neighborsSink.cell

      override def sensor[A](id: LocalSensorId): Cell[A] = id match
        case Source => new Cell(sources.contains(selfId).asInstanceOf[A])
        case Obstacle => new Cell(obstacles.contains(selfId).asInstanceOf[A])

    case class SimulationNeighborInfo(selfId: DeviceId, neighborId: DeviceId, exported: Export[Any]) extends BasicNeighborInfo:
      override def sensor[A](id: NeighborSensorId): A = id match
        case NbrRange => (environment.position(selfId), environment.position(neighborId)) match
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
