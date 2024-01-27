package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.test.utils.collections.Table.*

/** A [[ConvergenceTest]] for the nbr construct. */
class NbrTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Nbr = symbol("nbr")

  import DefaultSimulator.incarnation.{*, given}

  Nbr should "collect the computations of each device and its neighbors" in convergenceTest(
    simulator = DefaultSimulator,
    flow = countNeighbors,
    limit = Map(
      0 -> 4, 1 -> 6, 2 -> 4,
      3 -> 6, 4 -> 9, 5 -> 6,
      6 -> 4, 7 -> 6, 8 -> 4
    )
  )

  it should "work within other nbr constructs" in convergenceTest(
    simulator = DefaultSimulator,
    flow = countSharedNeighbors,
    limit = Map(
      0 -> Map(0 -> 4, 1 -> 4, 2 -> 2, 3 -> 4, 4 -> 4, 5 -> 2, 6 -> 2, 7 -> 2, 8 -> 1),
      1 -> Map(        1 -> 6, 2 -> 4, 3 -> 4, 4 -> 6, 5 -> 4, 6 -> 2, 7 -> 3, 8 -> 2),
      2 -> Map(                2 -> 4, 3 -> 2, 4 -> 4, 5 -> 4, 6 -> 1, 7 -> 2, 8 -> 2),
      3 -> Map(                        3 -> 6, 4 -> 6, 5 -> 3, 6 -> 4, 7 -> 4, 8 -> 2),
      4 -> Map(                                4 -> 9, 5 -> 6, 6 -> 4, 7 -> 6, 8 -> 4),
      5 -> Map(                                        5 -> 6, 6 -> 2, 7 -> 4, 8 -> 4),
      6 -> Map(                                                6 -> 4, 7 -> 4, 8 -> 2),
      7 -> Map(                                                        7 -> 6, 8 -> 4),
      8 -> Map(                                                                8 -> 4),
    ).withTransposed
  )

  it should "work in parallel with other nbr constructs" in convergenceTest(
    simulator = DefaultSimulator,
    flow = minMaxNeighborsId,
    limit = Map(
      0 -> (0, 4), 1 -> (0, 5), 2 -> (1, 5),
      3 -> (0, 7), 4 -> (0, 8), 5 -> (1, 8),
      6 -> (3, 7), 7 -> (3, 8), 8 -> (4, 8)
    )
  )

