package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the loop construct. */
class LoopTest extends ConvergenceTest.WithDefaults:
  private val Loop = symbol("loop")

  import defaultSimulator.incarnation.{*, given}

  private def count(from: Int, to: Int, step: Int = 1): Flow[Int] =
    loop(from - step){ _.map(x => math.min(x + step, to)) }
  Loop should "make the state of each device evolve in time" in convergenceTest(
    simulator = defaultSimulator,
    flow = count(from = 0, to = 10),
    limit = Seq.range(0, 9).map(_ -> 10).toMap
  )

  private def accumulate[T](flow: => Flow[T], memory: Int = Int.MaxValue, calm: Boolean = true): Flow[Seq[T]] =
    loop(Seq.empty){
      lift(_, flow){
        case (acc, next) if acc.lastOption.forall(!calm || _ != next) => (acc :+ next).takeRight(memory)
        case (acc, _) => acc
      }
    }
  it should "work within other loop constructs" in convergenceTest(
    simulator = defaultSimulator,
    flow = accumulate(count(from = 0, to = 3)),
    limit = Seq.range(0, 9).map(_ -> Seq(0, 1, 2, 3)).toMap
  )

  private def parallel[T](flows: Seq[Flow[T]]): Flow[Seq[T]] =
    flows.foldLeft(constant(Seq.empty))((acc, next) => lift(acc, next)(_ :+ _))
  private def replicate[T](flow: => Flow[T], replicas: Int): Flow[Seq[T]] =
    parallel(Seq.range(0, replicas).map(_ => flow))
  it should "work in parallel with other loop constructs" in convergenceTest(
    simulator = defaultSimulator,
    flow = replicate(flow = count(from = 0, to = 10), replicas = 3),
    limit = Seq.range(0, 9).map(_ -> Seq(10, 10, 10)).toMap
  )
