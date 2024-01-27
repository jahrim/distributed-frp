package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the loop construct. */
class LoopTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Loop = symbol("loop")

  import DefaultSimulator.incarnation.{*, given}

  Loop should "make the state of each device evolve in time" in convergenceTest(
    simulator = DefaultSimulator,
    flow = count(from = 0, to = 10),
    limit = Seq.range(0, environment.nDevices).map(_ -> 10).toMap
  )

  it should "work within other loop constructs" in convergenceTest(
    simulator = DefaultSimulator,
    flow = accumulate(count(from = 0, to = 3)),
    limit = Seq.range(0, environment.nDevices).map(_ -> Seq(0, 1, 2, 3)).toMap
  )

  it should "work in parallel with other loop constructs" in convergenceTest(
    simulator = DefaultSimulator,
    flow = replicate(flow = count(from = 0, to = 10), replicas = 3),
    limit = Seq.range(0, environment.nDevices).map(_ -> Seq(10, 10, 10)).toMap
  )
