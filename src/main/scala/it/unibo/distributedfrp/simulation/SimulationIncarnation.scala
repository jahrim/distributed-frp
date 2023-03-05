package it.unibo.distributedfrp.simulation

import it.unibo.distributedfrp.core.Incarnation
import it.unibo.distributedfrp.frp.IncrementalCellSink
import nz.sodium.Cell

import scala.math._

class SimulationIncarnation(environment: Environment,
                            sources: Set[Int] = Set.empty,
                            obstacles: Set[Int] = Set.empty)
  extends Incarnation
    with EnvironmentSimulation(environment)
    with TestLocalSensors
    with TestNeighborSensors:


  override type Context = SimulationContext
  override type NeighborState = SimulationNeighborState

  override def context(selfId: DeviceId): Context = new SimulationContext(selfId)

  class SimulationContext(val selfId: DeviceId) extends BasicContext:
    private val neighborsSink = new IncrementalCellSink[Map[DeviceId, NeighborState]](Map.empty, calm = true)

    def neighborExported(neighborId: DeviceId, exported: Export[Any]): Unit =
      neighborsSink.update(_ + (neighborId -> SimulationNeighborState(selfId, neighborId, exported)))

    override def neighbors: Cell[Map[DeviceId, NeighborState]] = neighborsSink.cell

    import SimulationLocalSensor._
    override def sensor[A](id: LocalSensorId): Cell[A] = id match
      case Source => new Cell(sources.contains(selfId).asInstanceOf[A])
      case Obstacle => new Cell(obstacles.contains(selfId).asInstanceOf[A])

  case class SimulationNeighborState(selfId: DeviceId, neighborId: DeviceId, exported: Export[Any]) extends BasicNeighborState:
    import SimulationNeighborSensor._
    override def sensor[A](id: NeighborSensorId): A = id match
      case NbrRange => (environment.position(selfId), environment.position(neighborId)) match
        case ((x1, y1), (x2, y2)) => hypot(x1 - x2, y1 - y2).asInstanceOf[A]