package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the withoutNeighbor construct. */
class WithoutNeighborTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val WithoutNeighbor = symbol("withoutNeighbor")

  import DefaultSimulator.incarnation.{*, given}

  WithoutNeighbor should
    "exclude the computation of a specific device from the " +
    "computations of the neighboring devices" in convergenceTest(
    simulator = DefaultSimulator,
    flow = collectNeighborsExcept(neighborId = 4),
    limit = Map(
      0 -> Set(0, 1, 3),       1 -> Set(0, 1, 2, 3, 5),          2 -> Set(1, 2, 5),
      3 -> Set(0, 1, 3, 6, 7), 4 -> Set(0, 1, 2, 3, 5, 6, 7, 8), 5 -> Set(1, 2, 5, 7, 8),
      6 -> Set(3, 6, 7),       7 -> Set(3, 5, 6, 7, 8),          8 -> Set(5, 7, 8)
    ),
  )
