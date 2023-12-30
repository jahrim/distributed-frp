package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation
import it.unibo.distributedfrp.simulation.sensors.LocalSensor

import scala.concurrent.Future

/** A [[ConvergenceTest]] for the sensor construct. */
class SensorTest extends ConvergenceTest.WithDefaults:
  private val Sensor = symbol("sensor")

  import defaultSimulator.incarnation.{*, given}

  private def isSource: Flow[Boolean] = sensor[Boolean](Source)
  Sensor should "make the devices perceive static information from their environment" in convergenceTest(
    simulator = defaultSimulator,
    flow = {
      summon[Environment].tag(tag = SourceTag, devices = Set(0, 1, 2))
      isSource
    },
    limit = Map(
      0 -> true,  1 -> true,  2 -> true,
      3 -> false, 4 -> false, 5 -> false,
      6 -> false, 7 -> false, 8 -> false
    )
  )

  private def isSourceOrObstacle: Flow[Option[Source.type | Obstacle.type]] =
    mux(isSource){ constant(Some(Source)) }{ mux(isObstacle){ constant(Some(Obstacle)) }{ constant(None) }}
  it should "make the devices perceive selective information from their environment" in convergenceTest(
    simulator = defaultSimulator,
    flow = {
      summon[Environment].setTags(tags = Map(SourceTag -> Set(0, 1, 2), ObstacleTag -> Set(3, 4, 5)))
      isSourceOrObstacle
    },
    limit = Map(
      0 -> Some(Source),   1 -> Some(Source),   2 -> Some(Source),
      3 -> Some(Obstacle), 4 -> Some(Obstacle), 5 -> Some(Obstacle),
      6 -> None,           7 -> None,           8 -> None
    )
  )

  private def sideEffect[T](flow: Flow[T], onExport: PartialFunction[T, Unit]): Flow[T] =
    flow.map(t => { if onExport.isDefinedAt(t) then Future(onExport(t)); t })
  private def sourceMid: Flow[DeviceId] = mux(isSource){ mid }{ constant(noId) }
  it should "make the devices perceive dynamic information from their environment" in convergenceTest(
    simulator = defaultSimulator,
    flow = {
      summon[Environment].tag(tag = SourceTag, devices = Set(0))
      sideEffect(
        flow = sourceMid,
        onExport = { case id if id < 4 => summon[Environment].tag(SourceTag, Set(id + 1)) }
      )
    },
    limit = Map(
      0 -> 0,    1 -> 1,    2 -> 2,
      3 -> 3,    4 -> 4,    5 -> noId,
      6 -> noId, 7 -> noId, 8 -> noId
    )
  )

  private object GPSSensor extends LocalSensor[(Double, Double)]:
    override type SuitableEnvironment = Environment
    override def setup[I <: SimulationIncarnation](incarnation: I)(using
      incarnation.Environment <:< SuitableEnvironment
    ): incarnation.SimulationLocalSensor[(Double, Double)] = owner =>
      new nz.sodium.Cell[(Double, Double)](owner.environment.position(owner.selfId))
  private object GPS extends LocalSensorId
  registerLocalSensors(GPS -> GPSSensor.setup(defaultSimulator.incarnation))

  private def position: Flow[(Double, Double)] = sensor[(Double, Double)](GPS)
  it should "let the user define custom sensors" in convergenceTest(
    simulator = defaultSimulator,
    flow = position,
    limit = Map(
      0 -> (0.0, 0.0), 1 -> (1.0, 0.0), 2 -> (2.0, 0.0),
      3 -> (0.0, 1.0), 4 -> (1.0, 1.0), 5 -> (2.0, 1.0),
      6 -> (0.0, 2.0), 7 -> (1.0, 2.0), 8 -> (2.0, 2.0)
    )
  )
