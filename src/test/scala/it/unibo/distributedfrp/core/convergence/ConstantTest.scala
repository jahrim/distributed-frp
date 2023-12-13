package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the constant construct. */
class ConstantTest extends ConvergenceTest:
  private val simulator = ConvergenceSimulator.DSL.default
  import simulator.{*, given}

  private val Constant = symbol("constant")

  Constant should "compute the specified value for each device" in
    Seq("A", "B", "C").foreach(value =>
      convergenceTest(
        simulator = simulator,
        flow = constant(value),
        limit = Map(
          0 -> value, 1 -> value, 2 -> value,
          3 -> value, 4 -> value, 5 -> value,
          6 -> value, 7 -> value, 8 -> value
        )
      )
    )

  it should "not compute any value different from the specified value" in convergenceTest(
    simulator = simulator,
    flow = constant("A"),
    limit = Map(
      0 -> "A", 1 -> "A", 2 -> "A",
      3 -> "A", 4 -> "A", 5 -> "A",
      6 -> "A", 7 -> "A", 8 -> "OtherValue"
    ),
    expectation = false
  )
