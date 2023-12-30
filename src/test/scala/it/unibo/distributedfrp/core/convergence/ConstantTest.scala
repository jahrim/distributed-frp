package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the constant construct. */
class ConstantTest extends ConvergenceTest.WithDefaults:
  private val Constant = symbol("constant")

  import defaultSimulator.incarnation.{*, given}

  Constant should "compute the specified integer value for each device" in
    Seq(0, 1, 2).foreach(integer =>
      convergenceTest(
        simulator = defaultSimulator,
        flow = constant(integer),
        limit = Seq.range(0, 9).map(_ -> integer).toMap
      )
    )

  private case class CustomObject(value: Int)
  it should "compute the specified custom value for each device" in
    Seq(0, 1, 2).map(CustomObject.apply).foreach(customValue =>
      convergenceTest(
        simulator = defaultSimulator,
        flow = constant(customValue),
        limit = Seq.range(0, 9).map(_ -> customValue).toMap
      )
    )
