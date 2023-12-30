package it.unibo.distributedfrp.simulation.incarnation

import it.unibo.distributedfrp.simulation.environment.EnvironmentWithTags
import it.unibo.distributedfrp.simulation.sensors.*

/**
 * A [[SimulationIncarnation]] extended with the most common sensors
 * in collective computing.
 */
trait CommonSensors:
  self: SimulationIncarnation =>

  override type Environment <: EnvironmentWithTags

  export ObstacleSensor.Tag as ObstacleTag
  export SourceSensor.Tag as SourceTag

  /** Id of the [[ObstacleSensor]]. */
  object Obstacle extends LocalSensorId
  /** Id of the [[SourceSensor]]. */
  object Source extends LocalSensorId
  /** Id of the [[NbrRangeSensor]]. */
  object NbrRange extends NeighborSensorId

  /**
   * For each device in the network, compute true if it is an obstacle, false otherwise.
   *
   * @note as [[sensor sensor(Obstacle)]].
   * @note devices in the environment can be tagged as obstacles using
   *       [[EnvironmentWithTags.tag EnvironmentWithTags.tag(ObstacleTag, Set[DeviceId])]].
   */
  def isObstacle: Flow[Boolean] = sensor[Boolean](Obstacle)

  /**
   * For each device in the network, compute true if it is a source, false otherwise.
   *
   * @note as [[sensor sensor(Source)]].
   * @note devices in the environment can be tagged as sources using
   *       [[EnvironmentWithTags.tag EnvironmentWithTags.tag(SourceTag, Set[DeviceId])]].
   */
  def isSource: Flow[Boolean] = sensor[Boolean](Source)

  /**
   * For each device in the network, compute the distances from all of its neighbors.
   *
   * @note as [[sensor sensor(NbrRange)]].
   */
  def nbrRange: Flow[NeighborField[Double]] = nbrSensor[Double](NbrRange)

  this.registerLocalSensors(
    Obstacle -> ObstacleSensor.setup(this),
    Source -> SourceSensor.setup(this)
  )
  this.registerNeighborSensors(
    NbrRange -> NbrRangeSensor.setup(this)
  )

/** Companion object of [[CommonSensors]]. */
object CommonSensors:
  /** The default configuration of [[CommonSensors]]. */
  trait Default extends CommonSensors:
    self: SimulationIncarnation =>
    override type Environment = EnvironmentWithTags
