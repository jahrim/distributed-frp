package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the toSet construct. */
class ToSetTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val ToSet = symbol("toSet")
  
  import DefaultSimulator.incarnation.{*, given}

  ToSet should "collect the computations of the neighboring devices in a set" in convergenceTest(
    simulator = DefaultSimulator,
    flow = collectNeighbors,
    limit = Map(
      0 -> Set(0, 1, 3, 4),       1 -> Set(0, 1, 2, 3, 4, 5),          2 -> Set(1, 2, 4, 5),
      3 -> Set(0, 1, 3, 4, 6, 7), 4 -> Set(0, 1, 2, 3, 4, 5, 6, 7, 8), 5 -> Set(1, 2, 4, 5, 7, 8),
      6 -> Set(3, 4, 6, 7),       7 -> Set(3, 4, 5, 6, 7, 8),          8 -> Set(4, 5, 7, 8)
    ),
  )
