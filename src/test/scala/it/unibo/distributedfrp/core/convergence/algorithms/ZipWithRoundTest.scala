package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest
import it.unibo.distributedfrp.simulation.environment.Environment.euclideanGrid
import it.unibo.distributedfrp.simulation.environment.{EnvironmentFactory, EnvironmentWithTags}

/** A [[ConvergenceTest]] for the round tracking algorithm. */
class ZipWithRoundTest extends ConvergenceTest.WithDefaults:
  private val ZipWithRound = symbol("zipWithRound")

  import defaultSimulator.incarnation.{*, given}
  private given EnvironmentFactory[EnvironmentWithTags] = () => EnvironmentWithTags(euclideanGrid(cols = 5, rows = 5))

  private def zipWithRound[T](flow: Flow[T], startingRound: Int = 0): Flow[(Int, T)] =
    loop((startingRound - 1, Option.empty[T])) {
      lift(_, flow, mid) {  // TODO remove mid (debug)
        case ((round, prev), next, id) if !prev.contains(next) =>
          if id == 0 then   // TODO remove (debug)
            println(s"[${Thread.currentThread().getName}] Next round: " + (round + 1, prev, next))
          (round + 1, Some(next))
        case (prevResult, _, id) =>
          if id == 0 then   // TODO remove (debug)
            println(s"[${Thread.currentThread().getName}] Same round: " + prevResult)
          prevResult
      }
    }.map(_ -> _.get)

  private def count(from: Int, to: Int, step: Int = 1): Flow[Int] =
    loop(from - step) { _.map(x => math.min(x + step, to)) }
  private def sequence[T](xs: T*): Flow[T] =
    count(from = 0, to = xs.size - 1).map(xs.apply)
  ZipWithRound should
    "zip the computations of each device in the network with the round when it was computed, " +
    "considering each round to happen for a device when the computation of the device changes" in convergenceTest(
    simulator = defaultSimulator,
    flow = zipWithRound(sequence(0, 0, 1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10, 10)),
    limit = Seq.range(0, 25).map(_ -> (10, 10)).toMap
  )

  // TODO why does the code below loop indefinitely?
  //      - `leaderElection` works by itself
  //      - `zipWithRound` works with `sequence`
  //      - seems related to a weird interaction with the
  //        `lift` and `nbr` operators within `loop`
  private def leaderElection: Flow[DeviceId] =
    loop(Int.MinValue) { leaderId => lift(mid, nbr(leaderId).max)(math.max) }
  it should "work for leader election also" in convergenceTest(
    simulator = defaultSimulator,
    flow = zipWithRound(leaderElection),
    limit = Map(
       0 -> (4, 24),  1 -> (4, 24),  2 -> (4, 24),  3 -> (4, 24),  4 -> (4, 24),
       5 -> (4, 24),  6 -> (3, 24),  7 -> (3, 24),  8 -> (3, 24),  9 -> (3, 24),
      10 -> (4, 24), 11 -> (3, 24), 12 -> (2, 24), 13 -> (2, 24), 14 -> (2, 24),
      15 -> (4, 24), 16 -> (3, 24), 17 -> (2, 24), 18 -> (1, 24), 19 -> (1, 24),
      20 -> (4, 24), 21 -> (3, 24), 22 -> (2, 24), 23 -> (1, 24), 24 -> (0, 24),
    )
  )
