package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.incarnation.Incarnation
import nz.sodium.{Cell, CellSink}

trait MockIncarnation extends Incarnation:
  override type DeviceId = Int
  override type SensorId = String
  override type Context = MockContext
  override type NeighborInfo = MockNeighborInfo

  def initialLocalSensors(selfId: DeviceId): Map[SensorId, Any]
  def initialNeighborSensors(selfId: DeviceId, neighborId: DeviceId): Map[SensorId, Any]

  override def context(selfId: DeviceId): Context = MockContext(selfId, initialLocalSensors(selfId))

  case class MockNeighborInfo(exported: Export[Any], sensors: Map[SensorId, Any]) extends BasicNeighborInfo:
    override def sensor[A](id: SensorId): A = sensors(id).asInstanceOf[A]

  class MockContext(val selfId: DeviceId, initialSensorValues: Map[SensorId, Any]) extends BasicContext:
    private var neighborsInfo: NeighborField[NeighborInfo] = NeighborField.empty
    private val neighborsInfoCell: CellSink[NeighborField[NeighborInfo]] = new CellSink(neighborsInfo)
    private val sensors: Map[SensorId, CellSink[Any]] = initialSensorValues.map((k, v) => (k, new CellSink[Any](v)))

    override def sensor[A](id: SensorId): Cell[A] = sensors(id).map(_.asInstanceOf[A])

    override def neighbors: Cell[NeighborField[NeighborInfo]] = neighborsInfoCell

    def updateLocalSensor(id: SensorId)(newValue: Any): Unit =
      sensors(id).send(newValue)

    def addNeighbor(neighborId: DeviceId)(exported: Export[Any]): Unit =
      updateNeighbors(_.withNeighbor(neighborId)(MockNeighborInfo(exported, initialNeighborSensors(selfId, neighborId))))

    def removeNeighbor(neighborId: DeviceId): Unit =
      updateNeighbors(_.withoutNeighbor(neighborId))

    def receiveExportFromNeighbor(neighborId: DeviceId)(exported: Export[Any]): Unit =
      updateNeighbors(_.updateNeighbor(neighborId)(_.copy(exported = exported)))

    def updateSensorForNeighbor(neighborId: DeviceId)(sensorId: SensorId, newValue: Any): Unit =
      updateNeighbors(_.updateNeighbor(neighborId)(x => x.copy(sensors = x.sensors + (sensorId -> newValue))))

    private def updateNeighbors(update: NeighborField[NeighborInfo] => NeighborField[NeighborInfo]): Unit =
      neighborsInfo = update(neighborsInfo)
      neighborsInfoCell.send(neighborsInfo)


