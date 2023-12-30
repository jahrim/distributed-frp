package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the max construct. */
class MaxTest extends ConvergenceTest.WithDefaults:
  private val Max = symbol("max")
  
  import defaultSimulator.incarnation.{*, given}

  private def maxNeighborId: Flow[Int] = nbr(mid).max
  Max should "evaluate the maximum of the computations of the neighboring devices" in convergenceTest(
    simulator = defaultSimulator,
    flow = maxNeighborId,
    limit = Map(
      0 -> 4, 1 -> 5, 2 -> 5,
      3 -> 7, 4 -> 8, 5 -> 8,
      6 -> 7, 7 -> 8, 8 -> 8
    )
  )

  private case class CustomId[I](value: I)
  private given LowerBounded[CustomId[Int]] = new LowerBounded[CustomId[Int]]:
    override def lowerBound: CustomId[Int] = CustomId(Int.MinValue)
    override def compare(x: CustomId[Int], y: CustomId[Int]): Int = summon[Ordering[Int]].compare(x.value, y.value)
  private def maxNeighborCustomId: Flow[CustomId[Int]] = nbr(mid.map(CustomId.apply)).max
  it should "evaluate the maximum for a custom type given its lower bound" in convergenceTest(
    simulator = defaultSimulator,
    flow = maxNeighborCustomId,
    limit = Map(
      0 -> CustomId(4), 1 -> CustomId(5), 2 -> CustomId(5),
      3 -> CustomId(7), 4 -> CustomId(8), 5 -> CustomId(8),
      6 -> CustomId(7), 7 -> CustomId(8), 8 -> CustomId(8)
    )
  )
