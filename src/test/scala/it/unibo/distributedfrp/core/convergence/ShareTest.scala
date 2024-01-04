package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the share construct. */
class ShareTest extends ConvergenceTest.WithDefaults:
  private val Share = symbol("share")

  import defaultSimulator.incarnation.{*, given}

  // From the paper: https://www.semanticscholar.org/reader/e56f679047e1c40482f8f04203c6abd275d77bf4
  private def ever(condition: Flow[Boolean]): Flow[Boolean] =
    share(constant(false)){ lift(_, condition)(_.values.foldLeft(_)(_ || _)) }
  Share should "make the state of neighboring devices evolve in time" in convergenceTest(
    simulator = defaultSimulator,
    flow = ever(mid.map(_ == 0)),
    limit = Seq.range(0, 9).map(_ -> true).toMap
  )

  private def sharedCount(from: Int, to: Int, step: Int = 1): Flow[Int] =
    val start = from - step
    share(constant(Int.MinValue)){ maxCounts =>
      val newCount = share(constant(start)) { counts =>
        lift(counts, mid)(_.getOrElse(_, start) + step).map(math.min(_, to))
      }
      lift(maxCounts.max, newCount)(math.max)
    }
  it should "work within other share constructs" in convergenceTest(
    simulator = defaultSimulator,
    flow = sharedCount(from = 0, to = 10),
    limit = Seq.range(0, 9).map(_ -> 10).toMap
  )

  private def parallelSharedCount(from: Int, to: Int, step: Int = 1): Flow[(Int, Int)] =
    lift(sharedCount(from, to, step), sharedCount(from, to, step))(_ -> _)
  it should "work in parallel with other share constructs" in convergenceTest(
    simulator = defaultSimulator,
    flow = parallelSharedCount(from = 0, to = 10),
    limit = Seq.range(0, 9).map(_ -> (10, 10)).toMap
  )
