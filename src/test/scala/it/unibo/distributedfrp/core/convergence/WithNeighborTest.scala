package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the withNeighbor construct. */
class WithNeighborTest extends ConvergenceTest.WithDefaults:
  private val WithNeighbor = symbol("withNeighbor")

  import defaultSimulator.incarnation.{*, given}

  private def collectNeighborsIncluding(neighborId: DeviceId): Flow[Set[DeviceId]] =
    nbr(mid).map(_.withNeighbor(neighborId, neighborId).values.toSet)
  WithNeighbor should
    "include the computation of a specific device from the " +
    "computations of the neighboring devices" in convergenceTest(
    simulator = defaultSimulator,
    flow = collectNeighborsIncluding(neighborId = 100),
    limit = Map(
      0 -> Set(0, 1, 3, 4, 100),       1 -> Set(0, 1, 2, 3, 4, 5, 100),          2 -> Set(1, 2, 4, 5, 100),
      3 -> Set(0, 1, 3, 4, 6, 7, 100), 4 -> Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 100), 5 -> Set(1, 2, 4, 5, 7, 8, 100),
      6 -> Set(3, 4, 6, 7, 100),       7 -> Set(3, 4, 5, 6, 7, 8, 100),          8 -> Set(4, 5, 7, 8, 100)
    ),
  )

  private def collectMaskedNeighbors(maskedId: DeviceId, mask: DeviceId): Flow[Set[DeviceId]] =
    nbr(mid).map(_.withNeighbor(maskedId, mask).values.toSet)
  it should
    "set the computation of a specific device from the " +
    "computations of the neighboring devices, if already present" in convergenceTest(
    simulator = defaultSimulator,
    flow = collectMaskedNeighbors(maskedId = 4, mask = 100),
    limit = Map(
      0 -> Set(0, 1, 3, 100),       1 -> Set(0, 1, 2, 3, 100, 5),          2 -> Set(1, 2, 100, 5),
      3 -> Set(0, 1, 3, 100, 6, 7), 4 -> Set(0, 1, 2, 3, 100, 5, 6, 7, 8), 5 -> Set(1, 2, 100, 5, 7, 8),
      6 -> Set(3, 100, 6, 7),       7 -> Set(3, 100, 5, 6, 7, 8),          8 -> Set(100, 5, 7, 8)
    ),
  )
