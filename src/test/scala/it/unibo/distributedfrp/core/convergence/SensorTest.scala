package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation
import it.unibo.distributedfrp.simulation.sensors.LocalSensor

/** A [[ConvergenceTest]] for the sensor construct. */
class SensorTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Sensor = symbol("sensor")

  import DefaultSimulator.incarnation.{*, given}

  Sensor should "make the devices perceive static information from their environment" in convergenceTest(
    simulator = DefaultSimulator,
    flow = {
      environment.tag(tag = SourceTag, devices = Set(0, 1, 2))
      isSource
    },
    limit = Map(
      0 -> true,  1 -> true,  2 -> true,
      3 -> false, 4 -> false, 5 -> false,
      6 -> false, 7 -> false, 8 -> false
    )
  )

  it should "make the devices perceive selective information from their environment" in convergenceTest(
    simulator = DefaultSimulator,
    flow = {
      environment.setTags(tags = Map(SourceTag -> Set(0, 1, 2), ObstacleTag -> Set(3, 4, 5)))
      assignSourceOrObstacleRoles
    },
    limit = Map(
      0 -> Source,   1 -> Source,   2 -> Source,
      3 -> Obstacle, 4 -> Obstacle, 5 -> Obstacle,
      6 -> None,     7 -> None,     8 -> None
    )
  )

  it should "make the devices perceive dynamic information from their environment" in convergenceTest(
    simulator = DefaultSimulator,
    flow = {
      environment.tag(tag = SourceTag, devices = Set(0))
      sideEffect(
        flow = sourceMid,
        onExport = {
          case Some(id) if id < 4 =>
            nz.sodium.Transaction.post(() => environment.tag(SourceTag, Set(id + 1)))
        }
      )
    },
    limit = Map(
      0 -> Some(0), 1 -> Some(1), 2 -> Some(2),
      3 -> Some(3), 4 -> Some(4), 5 -> None,
      6 -> None,    7 -> None,    8 -> None
    )
  )

  private object GPSSensor extends LocalSensor[(Double, Double)]:
    override type SuitableEnvironment = Environment
    override def setup[I <: SimulationIncarnation](incarnation: I)(using
      incarnation.Environment <:< SuitableEnvironment
    ): incarnation.SimulationLocalSensor[(Double, Double)] = owner =>
      new nz.sodium.Cell[(Double, Double)](owner.environment.position(owner.selfId))
  private object GPS extends LocalSensorId
  registerLocalSensors(GPS -> GPSSensor.setup(DefaultSimulator.incarnation))

  private def position: Flow[(Double, Double)] = sensor[(Double, Double)](GPS)

  it should "let the user define custom sensors" in convergenceTest(
    simulator = DefaultSimulator,
    flow = position,
    limit = Map(
      0 -> (0.0, 0.0), 1 -> (1.0, 0.0), 2 -> (2.0, 0.0),
      3 -> (0.0, 1.0), 4 -> (1.0, 1.0), 5 -> (2.0, 1.0),
      6 -> (0.0, 2.0), 7 -> (1.0, 2.0), 8 -> (2.0, 2.0)
    )
  )
