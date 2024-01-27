package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the mid construct. */
class MidTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Mid = symbol("mid")

  import DefaultSimulator.incarnation.{*, given}

  Mid should "compute the device id for each device" in convergenceTest(
    simulator = DefaultSimulator,
    flow = mid,
    limit = Seq.range(0, environment.nDevices).map(id => id -> id).toMap
  )
