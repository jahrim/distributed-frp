package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.utils.Liftable.map
import it.unibo.distributedfrp.utils.UpperBounded

/** A [[ConvergenceTest]] for the min construct. */
class MinTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Min = symbol("min")

  import DefaultSimulator.incarnation.{*, given}

  Min should "evaluate the minimum of the computations of the neighboring devices" in convergenceTest(
    simulator = DefaultSimulator,
    flow = minNeighborId,
    limit = Map(
      0 -> 0, 1 -> 0, 2 -> 1,
      3 -> 0, 4 -> 0, 5 -> 1,
      6 -> 3, 7 -> 3, 8 -> 4
    )
  )

  private case class CustomId[I](value: I)
  private given UpperBounded[CustomId[Int]] = new UpperBounded[CustomId[Int]]:
    override def upperBound: CustomId[Int] = CustomId[Int](Int.MaxValue)
    override def compare(x: CustomId[Int], y: CustomId[Int]): Int = summon[Ordering[Int]].compare(x.value, y.value)
  private def minNeighborCustomId: Flow[CustomId[Int]] = nbr(mid.map(CustomId.apply)).min

  it should "evaluate the minimum for a custom type given its lower bound" in convergenceTest(
    simulator = DefaultSimulator,
    flow = minNeighborCustomId,
    limit = Map(
      0 -> CustomId(0), 1 -> CustomId(0), 2 -> CustomId(1),
      3 -> CustomId(0), 4 -> CustomId(0), 5 -> CustomId(1),
      6 -> CustomId(3), 7 -> CustomId(3), 8 -> CustomId(4)
    )
  )
