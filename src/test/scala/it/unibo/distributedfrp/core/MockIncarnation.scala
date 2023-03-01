package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.frp.IncrementalCellSink
import nz.sodium.{Cell, CellSink}

trait MockIncarnation extends Incarnation:
  override type DeviceId = Int
  override type LocalSensorId = String
  override type NeighborSensorId = String
  override type Context = MockContext
  override type NeighborInfo = MockNeighborInfo

  def initialLocalSensors(selfId: DeviceId): Map[LocalSensorId, Any]
  def initialNeighborSensors(selfId: DeviceId, neighborId: DeviceId): Map[NeighborSensorId, Any]

  override def context(selfId: DeviceId): Context = MockContext(selfId, initialLocalSensors(selfId))

  case class MockNeighborInfo(exported: Export[Any], sensors: Map[NeighborSensorId, Any]) extends BasicNeighborInfo:
    override def sensor[A](id: NeighborSensorId): A = sensors(id).asInstanceOf[A]

  class MockContext(val selfId: DeviceId, initialSensorValues: Map[LocalSensorId, Any]) extends BasicContext:
    private val neighborsInfo: IncrementalCellSink[Map[DeviceId, NeighborInfo]] = new IncrementalCellSink(Map.empty)
    private val sensors: Map[LocalSensorId, CellSink[Any]] = initialSensorValues.map((k, v) => (k, new CellSink[Any](v)))

    override def sensor[A](id: LocalSensorId): Cell[A] = sensors(id).map(_.asInstanceOf[A])

    override def neighbors: Cell[Map[DeviceId, NeighborInfo]] = neighborsInfo.cell

    def updateLocalSensor(id: LocalSensorId)(newValue: Any): Unit =
      sensors(id).send(newValue)

    def addNeighbor(neighborId: DeviceId)(exported: Export[Any]): Unit =
      neighborsInfo.update(_ + (neighborId -> MockNeighborInfo(exported, initialNeighborSensors(selfId, neighborId))))

    def removeNeighbor(neighborId: DeviceId): Unit =
      neighborsInfo.update(_ - neighborId)

    def receiveExportFromNeighbor(neighborId: DeviceId)(exported: Export[Any]): Unit =
      neighborsInfo.update(_.updatedWith(neighborId)(_.map(_.copy(exported = exported))))

    def updateSensorForNeighbor(neighborId: DeviceId)(sensorId: NeighborSensorId, newValue: Any): Unit =
      neighborsInfo.update(_.updatedWith(neighborId)(_.map(x => x.copy(sensors = x.sensors + (sensorId -> newValue)))))


