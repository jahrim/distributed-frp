package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest

/** A [[ConvergenceTest]] for the network snapshot algorithm. */
class SnapshotTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Snapshot = symbol("snapshot")

  import DefaultSimulator.incarnation.{*, given}

  Snapshot should "collect the computation of all the devices in a network" in convergenceTest(
    simulator = DefaultSimulator,
    flow = snapshot(count(from = 0, to = 10)),
    limit = {
      val (lastTime, lastEvent) = (10, 10)
      val snapshot: Map[DeviceId, (Int, Int)] = Set.range(0, environment.nDevices).map(_ -> (lastTime, lastEvent)).toMap
      Seq.range(0, environment.nDevices).map(_ -> snapshot).toMap
    }
  )
