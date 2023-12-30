package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the mid construct. */
class MidTest extends ConvergenceTest.WithDefaults:
  private val Mid = symbol("mid")

  import defaultSimulator.incarnation.{*, given}

  Mid should "compute the device id for each device" in convergenceTest(
    simulator = defaultSimulator,
    flow = mid,
    limit = Seq.range(0, 9).map(id => id -> id).toMap
  )
