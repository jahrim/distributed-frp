package it.unibo.distributedfrp.core.convergence

import scala.concurrent.duration.*

/** A [[ConvergenceTest]] for the lift construct. */
class LiftTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Lift = symbol("lift")

  import DefaultSimulator.incarnation.{*, given}

  Lift should "combine the results of two flows" in convergenceTest(
    simulator = DefaultSimulator,
    flow = successorNeighbor,
    limit = Map(
      0 -> Some(1), 1 -> Some(2), 2 -> Some(4),
      3 -> Some(4), 4 -> Some(5), 5 -> Some(7),
      6 -> Some(7), 7 -> Some(8), 8 -> None,
    )
  )

  it should "combine the results of three flows" in convergenceTest(
    simulator = DefaultSimulator,
    flow = {
      environment.tag(tag = SourceTag, devices = Set(3, 4, 5))
      successorSourceNeighbor
    },
    limit = Map(
      0 -> Some(3), 1 -> Some(3), 2 -> Some(4),
      3 -> Some(4), 4 -> Some(5), 5 -> None,
      6 -> None,    7 -> None,    8 -> None,
    )
  )

  it should "work within other lift constructs" in convergenceTest(
    simulator = DefaultSimulator,
    flow = {
      environment.tag(tag = SourceTag, devices = Set(3, 4, 5))
      successorSourceNeighborNested
    },
    limit = Map(
      0 -> Some(3), 1 -> Some(3), 2 -> Some(4),
      3 -> Some(4), 4 -> Some(5), 5 -> None,
      6 -> None,    7 -> None,    8 -> None,
    )
  )

  it should "work in a dynamic environment" in convergenceTest(
    simulator = DefaultSimulator,
    flow = {
      environment.tag(tag = SourceTag, devices = Set.range(0, 9))
      sideEffect(
        flow = successorSourceNeighbor,
        onExport = {
          case Some(sourceNeighbor) =>
            nz.sodium.Transaction.post(() => environment.untag(tag = SourceTag, devices = Set(sourceNeighbor)))
        },
      )
    },
    limit = Seq.range(0, environment.nDevices).map(_ -> None).toMap
  )

  it should "be at least clear when it would loop a program indefinitely" in pendingUntilFixed(
    convergenceTest(
      simulator = DefaultSimulator,
      flow = zipWithRound(leaderElection),
      limit = Map(
         0 -> (2, 8),  1 -> (2, 8),  2 -> (2, 8),
         3 -> (2, 8),  4 -> (1, 8),  5 -> (1, 8),
         6 -> (2, 8),  7 -> (1, 8),  8 -> (0, 8),
      ),
      timeout = 10.seconds
    )
  )
