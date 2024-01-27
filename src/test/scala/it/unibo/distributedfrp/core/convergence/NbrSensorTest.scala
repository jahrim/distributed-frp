package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation
import it.unibo.distributedfrp.simulation.sensors.NeighborSensor
import it.unibo.distributedfrp.test.utils.collections.Table.*
import it.unibo.distributedfrp.utils.Liftable.map

/** A [[ConvergenceTest]] for the nbrSensor construct. */
class NbrSensorTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val NbrSensor = symbol("nbrSensor")

  import DefaultSimulator.incarnation.{*, given}

  NbrSensor should
    "make the devices perceive static relationships with their neighbors" +
    "from their environment" in convergenceTest(
    simulator = DefaultSimulator,
    flow = collectNeighborDistances.map(_.map(_ -> _.round(precision = 2))),
    limit = Map(
      0 -> Map(0 -> 0.00, 1 -> 1.00,            3 -> 1.00, 4 -> 1.41),
      1 -> Map(           1 -> 0.00, 2 -> 1.00, 3 -> 1.41, 4 -> 1.00, 5 -> 1.41),
      2 -> Map(                      2 -> 0.00,            4 -> 1.41, 5 -> 1.00),
      3 -> Map(                                 3 -> 0.00, 4 -> 1.00,            6 -> 1.00, 7 -> 1.41),
      4 -> Map(                                            4 -> 0.00, 5 -> 1.00, 6 -> 1.41, 7 -> 1.00, 8 -> 1.41),
      5 -> Map(                                                       5 -> 0.00,            7 -> 1.41, 8 -> 1.00),
      6 -> Map(                                                                  6 -> 0.00, 7 -> 1.00),
      7 -> Map(                                                                             7 -> 0.00, 8 -> 1.00),
      8 -> Map(                                                                                        8 -> 0.00)
    ).withTransposed
  )

  private object DirectionalSensor extends NeighborSensor[(Double, Double)]:
    override type SuitableEnvironment = Environment
    override def setup[I <: SimulationIncarnation](incarnation: I)(using
      incarnation.Environment <:< SuitableEnvironment
    ): incarnation.SimulationNeighborSensor[(Double, Double)] = neighborState =>
      val selfPosition: (Double, Double) = neighborState.environment.position(neighborState.selfId)
      val neighborPosition: (Double, Double) = neighborState.environment.position(neighborState.neighborId)
      (neighborPosition._1 - selfPosition._1, neighborPosition._2 - selfPosition._2)
  private object Direction extends NeighborSensorId
  registerNeighborSensors(Direction -> DirectionalSensor.setup(DefaultSimulator.incarnation))

  private def nbrDirection: Flow[NeighborField[(Double, Double)]] = nbrSensor[(Double, Double)](Direction)

  it should "let the user define custom sensors" in convergenceTest(
    simulator = DefaultSimulator,
    flow = nbrDirection,
    limit = Map(
      0 -> Map(0->(0,0),   1->(1,0),               3->(0,1),   4->(1,1)),
      1 -> Map(0->(-1,0),  1->(0,0),   2->(1,0),   3->(-1,1),  4->(0,1),   5->(1,1)),
      2 -> Map(            1->(-1,0),  2->(0,0),               4->(-1,1),  5->(0,1)),
      3 -> Map(0->(0,-1),  1->(1,-1),              3->(0,0),   4->(1,0),              6->(0,1),   7->(1,1)),
      4 -> Map(0->(-1,-1), 1->(0,-1),  2->(1,-1),  3->(-1,0),  4->(0,0),   5->(1,0),  6->(-1,1),  7->(0,1),   8->(1,1)),
      5 -> Map(            1->(-1,-1), 2->(0,-1),              4->(-1,0),  5->(0,0),              7->(-1,1),  8->(0,1)),
      6 -> Map(                                    3->(0,-1),  4->(1,-1),             6->(0,0),   7->(1,0)),
      7 -> Map(                                    3->(-1,-1), 4->(0,-1),  5->(1,-1), 6->(-1,0),  7->(0,0),   8->(1,0)),
      8 -> Map(                                                4->(-1,-1), 5->(0,-1),             7->(-1,0),  8->(0,0)),
    )
  )
