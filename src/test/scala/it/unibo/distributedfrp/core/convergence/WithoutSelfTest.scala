package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the withoutSelf construct. */
class WithoutSelfTest extends ConvergenceTest.WithDefaults:
  private val WithoutSelf = symbol("withoutSelf")

  import defaultSimulator.incarnation.{*, given}

  private def collectNeighborsExceptSelf: Flow[Set[DeviceId]] = nbr(mid).withoutSelf.toSet
  WithoutSelf should
    "exclude the computation of a device from the " +
    "computations of its neighboring devices" in convergenceTest(
    simulator = defaultSimulator,
    flow = collectNeighborsExceptSelf,
    limit = Map(
      0 -> Set(1, 3, 4),       1 -> Set(0, 2, 3, 4, 5),          2 -> Set(1, 4, 5),
      3 -> Set(0, 1, 4, 6, 7), 4 -> Set(0, 1, 2, 3, 5, 6, 7, 8), 5 -> Set(1, 2, 4, 7, 8),
      6 -> Set(3, 4, 7),       7 -> Set(3, 4, 5, 6, 8),          8 -> Set(4, 5, 7)
    ),
  )
