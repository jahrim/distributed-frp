package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the mid construct. */
class MidTest extends ConvergenceTest:
  private val simulator = ConvergenceSimulator.DSL.default
  import simulator.{*, given}

  private val Mid = symbol("mid")

  Mid should "compute the device id for each device" in convergenceTest(
    simulator = simulator,
    flow = mid,
    limit = Map(
      0 -> 0, 1 -> 1, 2 -> 2,
      3 -> 3, 4 -> 4, 5 -> 5,
      6 -> 6, 7 -> 7, 8 -> 8
    )
  )

  it should "not compute any value different from the device id" in convergenceTest(
    simulator = simulator,
    flow = mid,
    limit = Map(
      0 -> 0, 1 -> 1, 2 -> 2,
      3 -> 3, 4 -> 4, 5 -> 5,
      6 -> 6, 7 -> 7, 8 -> 1000
    ),
    expectation = false
  )
