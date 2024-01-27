package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest
import it.unibo.distributedfrp.simulation.environment.{Environment, EnvironmentWithTags}

/** A [[ConvergenceTest]] for the minimum channel algorithm. */
class MinChannelTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val MinChannel = symbol("minChannel")
  private val MinRedundantChannel = symbol("minRedundantChannel")

  import DefaultSimulator.incarnation.{Environment as _, *, given}
  override protected def defaultEnvironment: EnvironmentWithTags =
    EnvironmentWithTags(Environment.euclideanGrid(5, 5))

  private val channel: Boolean = true
  MinChannel should "compute the shortest path from a single source to a single destination" in convergenceTest(
    simulator = DefaultSimulator,
    flow = minChannelFromIds(sources = Set(0), destinations = Set(24)),
    limit = Map(
       0 -> channel,  1 -> false,    2 -> false,    3 -> false,    4 -> false,
       5 -> false,    6 -> channel,  7 -> false,    8 -> false,    9 -> false,
      10 -> false,   11 -> false,   12 -> channel, 13 -> false,   14 -> false,
      15 -> false,   16 -> false,   17 -> false,   18 -> channel, 19 -> false,
      20 -> false,   21 -> false,   22 -> false,   23 -> false,   24 -> channel,
    )
  )
  it should "compute the shortest path from one of multiple sources to a single destination" in convergenceTest(
    simulator = DefaultSimulator,
    flow = minChannelFromIds(sources = Set(0, 20), destinations = Set(24)),
    limit = Map(
       0 -> false,    1 -> false,    2 -> false,    3 -> false,    4 -> false,
       5 -> false,    6 -> false,    7 -> false,    8 -> false,    9 -> false,
      10 -> false,   11 -> false,   12 -> false,   13 -> false,   14 -> false,
      15 -> false,   16 -> false,   17 -> false,   18 -> false,   19 -> false,
      20 -> channel, 21 -> channel, 22 -> channel, 23 -> channel, 24 -> channel,
    )
  )

  MinRedundantChannel should
    "compute the shortest paths from a single source to a single destination, " +
    "within a margin of deviation from the actual shortest path" in convergenceTest(
    simulator = DefaultSimulator,
    flow = minRedundantChannelFromIds(sources = Set(0), destinations = Set(24), maxDeviation = constant(1.0)),
    limit = Map(
       0 -> channel,  1 -> channel,  2 -> false,    3 -> false,    4 -> false,
       5 -> channel,  6 -> channel,  7 -> channel,  8 -> false,    9 -> false,
      10 -> false,   11 -> channel, 12 -> channel, 13 -> channel, 14 -> false,
      15 -> false,   16 -> false,   17 -> channel, 18 -> channel, 19 -> channel,
      20 -> false,   21 -> false,   22 -> false,   23 -> channel, 24 -> channel,
    )
  )
