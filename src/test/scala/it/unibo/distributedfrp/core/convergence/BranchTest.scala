package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.core.FraspSamples

/** A [[ConvergenceTest]] for the branch construct. */
class BranchTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Branch = symbol("branch")

  import DefaultSimulator.incarnation.{*, given}

  Branch should "make the computation of the devices diverge conditionally" in convergenceTest(
    simulator = DefaultSimulator,
    flow = branchConditionally(selector = _ > 4, ifSelected = "Then", otherwise = "Else"),
    limit = Map(
      0 -> "Else", 1 -> "Else", 2 -> "Else",
      3 -> "Else", 4 -> "Else", 5 -> "Then",
      6 -> "Then", 7 -> "Then", 8 -> "Then"
    )
  )

  it should "split the network of the devices into subnetworks conditionally" in convergenceTest(
    simulator = DefaultSimulator,
    flow = splitNetworkAndCollectSortedNeighbors(boundary = _ > 2),
    limit = Map(
      0 -> Seq(0, 1),       1 -> Seq(0, 1, 2),          2 -> Seq(1, 2),
      3 -> Seq(3, 4, 6, 7), 4 -> Seq(3, 4, 5, 6, 7, 8), 5 -> Seq(4, 5, 7, 8),
      6 -> Seq(3, 4, 6, 7), 7 -> Seq(3, 4, 5, 6, 7, 8), 8 -> Seq(4, 5, 7, 8)
    )
  )

  // TODO Solve related issue: https://github.com/cric96/distributed-frp/issues/1
  it should "not split the network of the devices into subnetworks retroactively" in pendingUntilFixed(
    convergentEquivalenceTest(
      simulator = DefaultSimulator,
      flows = Seq(
        splitNetworkAndCollectSortedNeighbors(boundary = _ > 2),
        collectSortedNeighborsAndSplitNetwork(boundary = _ > 2)
      ),
      expectation = Expectation.Divergent
    )
  )

  import FraspSamples.BinaryDecisionTree as Split
  it should "split subnetworks of devices into other subnetworks of devices conditionally" in convergenceTest(
    simulator = DefaultSimulator,
    // Subnetworks: (0); (1,2); (3); (4,5); (6,7); (8)
    flow = splitNetworkRecursivelyAndCollectSortedNeighbors(
      boundaryPolicy = Split(
        decision = _ < 3,
        left = Some(Split(decision = _ < 1)),
        right = Some(Split(
          decision = _ < 6,
          left = Some(Split(decision = _ < 4)),
          right = Some(Split(decision = _ > 7)),
        ))
      )
    ),
    limit = Map(
      0 -> Seq(0),    1 -> Seq(1, 2), 2 -> Seq(1, 2),
      3 -> Seq(3),    4 -> Seq(4, 5), 5 -> Seq(4, 5),
      6 -> Seq(6, 7), 7 -> Seq(6, 7), 8 -> Seq(8)
    )
  )

  it should "split the network of devices into overlapping subnetworks of devices conditionally" in convergenceTest(
    simulator = DefaultSimulator,
    // Subnetworks: (0,1,2,3); (4,5,6,7,8); (0,2,4,6,8); (1,3,5,7,9); (1,4,7); (0,2,3,5,6,8)
    flow = splitNetworkWithOverlapsAndCollectSortedNeighbors(boundaries = (_ < 4), (_ % 2 == 0), Seq(1, 4, 7).contains),
    limit = Map(
      0 -> List(Seq(0, 1, 3),       Seq(0, 4),          Seq(0, 3)),
      1 -> List(Seq(0, 1, 2, 3),    Seq(1, 3, 5),       Seq(1, 4)),
      2 -> List(Seq(1, 2),          Seq(2, 4),          Seq(2, 5)),
      3 -> List(Seq(0, 1, 3),       Seq(1, 3, 7),       Seq(0, 3, 6)),
      4 -> List(Seq(4, 5, 6, 7, 8), Seq(0, 2, 4, 6, 8), Seq(1, 4, 7)),
      5 -> List(Seq(4, 5, 7, 8),    Seq(1, 5, 7),       Seq(2, 5, 8)),
      6 -> List(Seq(4, 6, 7),       Seq(4, 6),          Seq(3, 6)),
      7 -> List(Seq(4, 5, 6, 7, 8), Seq(3, 5, 7),       Seq(4, 7)),
      8 -> List(Seq(4, 5, 7, 8),    Seq(4, 8),          Seq(5, 8))
    )
  )
