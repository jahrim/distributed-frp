package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.utils.Liftable.map

/** A [[ConvergenceTest]] for the share construct. */
class ShareTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Share = symbol("share")

  import DefaultSimulator.incarnation.{*, given}

  Share should "make the state of neighboring devices evolve in time" in convergenceTest(
    simulator = DefaultSimulator,
    flow = ever(mid.map(_ == 0)),
    limit = Seq.range(0, environment.nDevices).map(_ -> true).toMap
  )

  it should "work within other share constructs" in convergenceTest(
    simulator = DefaultSimulator,
    flow = sharedCount(from = 0, to = 10),
    limit = Seq.range(0, environment.nDevices).map(_ -> 10).toMap
  )

  it should "work in parallel with other share constructs" in convergenceTest(
    simulator = DefaultSimulator,
    flow = replicate(sharedCount(from = 0, to = 10), replicas = 2),
    limit = Seq.range(0, environment.nDevices).map(_ -> Seq(10, 10)).toMap
  )
