package it.unibo.distributedfrp.simulation.incarnation

import it.unibo.distributedfrp.core.Incarnation
import it.unibo.distributedfrp.frp.IncrementalCellSink
import it.unibo.distributedfrp.simulation.environment
import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation.SensorNotFoundException
import nz.sodium.Cell

/**
 * An [[Incarnation]] that can be used to simulate the execution of
 * FRASP programs on a network of devices situated in a specific type
 * of [[environment.Environment Environment]].
 */
trait SimulationIncarnation extends Incarnation:
  override type DeviceId = Int
  override type Context = SimulationContext
  override type NeighborState = SimulationNeighborState
  override type LocalSensorId = SimulationLocalSensorId
  override type NeighborSensorId = SimulationNeighborSensorId
  /**
   * The type of [[environment.Environment Environment]] where the
   * devices of this [[SimulationIncarnation]] are situated.
   */
  type Environment <: environment.Environment

  /** The identifier of a [[SimulationLocalSensor SimulationLocalSensor]]. */
  trait SimulationLocalSensorId
  /** The identifier of a [[SimulationNeighborSensor SimulationNeighborSensor]]. */
  trait SimulationNeighborSensorId
  /**
   * A local sensor that can be used by a specific device to perceive a
   * specific type of readings from its [[Environment Environment]].
   *
   * @tparam R the type of readings perceived by this sensor.
   */
  @FunctionalInterface
  trait SimulationLocalSensor[R] { def sense(owner: Context): Cell[R] }
  /**
   * A neighbor sensor that can be used by a specific device to perceive a
   * specific type of readings from the states of its neighbors.
   *
   * @tparam R the type of readings perceived by this sensor.
   */
  @FunctionalInterface
  trait SimulationNeighborSensor[R] { def sense(neighborState: NeighborState): R }

  private var localSensors: Map[LocalSensorId, SimulationLocalSensor[?]] = Map.empty
  private var neighborSensors: Map[NeighborSensorId, SimulationNeighborSensor[?]] = Map.empty

  /**
   * Register the specified sensors as local sensors available within
   * the [[sensor sensor]] construct under the corresponding identifiers.
   *
   * @param sensors the specified sensors bound to the corresponding identifiers.
   */
  def registerLocalSensors(sensors: (LocalSensorId, SimulationLocalSensor[?])*): Unit =
    this.localSensors = this.localSensors ++ sensors
  /**
   * Register the specified sensors as neighbor sensors available within
   * the [[nbrSensor nbrSensor]] construct under the corresponding identifiers.
   *
   * @param sensors the specified sensors bound to the corresponding identifiers.
   */
  def registerNeighborSensors(sensors: (NeighborSensorId, SimulationNeighborSensor[?])*): Unit =
    this.neighborSensors = this.neighborSensors ++ sensors

  /**
   * A device in a FRASP simulation.
   *
   * @param selfId      the identifier of this device.
   * @param environment the environment where this device is situated.
   */
  class SimulationContext(override val selfId: DeviceId, val environment: Environment) extends BasicContext:
    private val neighborsSink = new IncrementalCellSink[Map[DeviceId, NeighborState]](Map.empty, calm = true)
    override def neighbors: Cell[Map[DeviceId, NeighborState]] = this.neighborsSink.cell
    override def sensor[A](id: LocalSensorId): Cell[A] = findLocalSensor[A](id).sense(owner = this)
    def receiveExport(neighborId: DeviceId, exported: Export[Any]): Unit =
      this.neighborsSink.update(_ + (neighborId -> SimulationNeighborState(selfId, neighborId, exported, environment)))
    private def findLocalSensor[A](id: LocalSensorId): SimulationLocalSensor[A] =
      localSensors.getOrElse(id, throw SensorNotFoundException(
        s"Sensor $id not found among the registered local sensors: $localSensors."
      )).asInstanceOf[SimulationLocalSensor[A]]

  /**
   * The state of a neighbor of a specific device.
   *
   * @param selfId      the identifier of the device observing its neighboring devices.
   * @param neighborId  the identifier of the neighboring device.
   * @param exported    the computation of the neighboring device.
   * @param environment the [[Environment]] where both devices are situated.
   */
  case class SimulationNeighborState(
    selfId: DeviceId,
    neighborId: DeviceId,
    exported: Export[Any],
    environment: Environment,
  ) extends BasicNeighborState:
    override def sensor[A](id: NeighborSensorId): A = findNeighborSensor[A](id).sense(neighborState = this)
    private def findNeighborSensor[A](id: NeighborSensorId): SimulationNeighborSensor[A] =
      neighborSensors.getOrElse(id, throw SensorNotFoundException(
        s"Sensor $id not found among the registered neighbor sensors: $neighborSensors."
      )).asInstanceOf[SimulationNeighborSensor[A]]

  /**
   * TODO consider changing the design of Incarnation.context(DeviceId)
   *      (move up the concept of environment by one abstraction layer)?
   */
  override def context(selfId: DeviceId): Context =
    throw UnsupportedOperationException("A device should be situated in an environment.")
  /**
   * @param selfId      the specified identifier.
   * @param environment the specified [[Environment Environment]].
   * @return a new device with the specified identifier situated in the
   *         specified [[Environment Environment]].
   */
  def context(selfId: DeviceId, environment: Environment): Context = SimulationContext(selfId, environment)

/** Companion object [[SimulationIncarnation]]. */
object SimulationIncarnation:
  /**
   * An [[Exception]] triggered when a sensor is queried but not found
   * among the registered sensors of this [[SimulationIncarnation]].
   */
  case class SensorNotFoundException(message: String) extends Exception(message)
