package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest
import it.unibo.distributedfrp.simulation.environment.{Environment, EnvironmentWithTags}
import it.unibo.distributedfrp.utils.Liftable.map

/** A [[ConvergenceTest]] for the gradient algorithm. */
class GradientTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Gradient = symbol("gradient")

  import DefaultSimulator.incarnation.{Environment as _, *, given}
  override protected def defaultEnvironment: EnvironmentWithTags =
    EnvironmentWithTags(Environment.euclideanGrid(5, 5))

  private val source: Double = 0.0
  Gradient should "compute the network distance from a single source for each device" in convergenceTest(
    simulator = DefaultSimulator,
    flow = {
      environment.tag(SourceTag, Set(0))
      gradient(isSource).map(_.round(precision = 2))
    },
    limit = Map(
       0 -> source,  1 -> 1,     2 -> 2,     3 -> 3,     4 -> 4,
       5 -> 1,       6 -> 1.41,  7 -> 2.41,  8 -> 3.41,  9 -> 4.41,
      10 -> 2,      11 -> 2.41, 12 -> 2.83, 13 -> 3.83, 14 -> 4.83,
      15 -> 3,      16 -> 3.41, 17 -> 3.83, 18 -> 4.24, 19 -> 5.24,
      20 -> 4,      21 -> 4.41, 22 -> 4.83, 23 -> 5.24, 24 -> 5.66,
    )
  )
  it should "compute the minimum network distance from any sources for each device" in convergenceTest(
    simulator = DefaultSimulator,
    flow = {
      environment.tag(SourceTag, Set(0, 4, 20, 24))
      gradient(isSource).map(_.round(precision = 2))
    },
    limit = Map(
       0 -> source,  1 -> 1,     2 -> 2,     3 -> 1,     4 -> source,
       5 -> 1,       6 -> 1.41,  7 -> 2.41,  8 -> 1.41,  9 -> 1,
      10 -> 2,      11 -> 2.41, 12 -> 2.83, 13 -> 2.41, 14 -> 2,
      15 -> 1,      16 -> 1.41, 17 -> 2.41, 18 -> 1.41, 19 -> 1,
      20 -> source, 21 -> 1,    22 -> 2,    23 -> 1,    24 -> source,
    )
  )

  private val obstacle: Double = -1
  private val unreachable: Double = Double.PositiveInfinity
  it should
    "compute the minimum network distance from any sources " +
    "for each device while avoiding obstacles" in convergenceTest(
    simulator = DefaultSimulator,
    flow = {
      environment.setTags(Map(SourceTag -> Set(0, 20), ObstacleTag -> Set(2, 7, 8, 9, 12, 17)))
      gradientWithObstacles(
        sources = isSource,
        obstacles = isObstacle,
        obstacleValue = obstacle
      ).map(_.round(precision = 2))
    },
    limit = Map(
       0 -> source,  1 -> 1,     2 -> obstacle,  3 -> unreachable,  4 -> unreachable,
       5 -> 1,       6 -> 1.41,  7 -> obstacle,  8 -> obstacle,     9 -> obstacle,
      10 -> 2,      11 -> 2.41, 12 -> obstacle, 13 -> 4.41,        14 -> 4.83,
      15 -> 1,      16 -> 1.41, 17 -> obstacle, 18 -> 3.41,        19 -> 4.41,
      20 -> source, 21 -> 1,    22 -> 2,        23 -> 3,           24 -> 4,
    )
  )
