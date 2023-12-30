package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the mux construct. */
class MuxTest extends ConvergenceTest.WithDefaults:
  private val Mux = symbol("mux")

  import defaultSimulator.incarnation.{*, given}

  private def divergeConditionally[A](selector: DeviceId => Boolean, ifSelected: => A, otherwise: => A): Flow[A] =
    mux(mid.map(selector)){ constant(ifSelected) } { constant(otherwise) }

  Mux should "make the computation of the devices diverge conditionally" in convergenceTest(
    simulator = defaultSimulator,
    flow = divergeConditionally(selector = _ > 4, ifSelected = "Then", otherwise = "Else"),
    limit = Map(
      0 -> "Else", 1 -> "Else", 2 -> "Else",
      3 -> "Else", 4 -> "Else", 5 -> "Then",
      6 -> "Then", 7 -> "Then", 8 -> "Then"
    )
  )

  private def divergeAndCollectSortedNeighbors(boundary: DeviceId => Boolean): Flow[Seq[DeviceId]] =
    def collectSortedNeighbors: Flow[Seq[DeviceId]] = nbr(mid).map(_.values.toSeq.sorted)
    mux(mid.map(boundary)){ collectSortedNeighbors }{ collectSortedNeighbors }
  it should "not split the network of the devices into subnetworks conditionally" in convergenceTest(
    simulator = defaultSimulator,
    flow = divergeAndCollectSortedNeighbors(boundary = _ > 2),
    limit = Map(
      0 -> Seq(0, 1, 3, 4),       1 -> Seq(0, 1, 2, 3, 4, 5),          2 -> Seq(1, 2, 4, 5),
      3 -> Seq(0, 1, 3, 4, 6, 7), 4 -> Seq(0, 1, 2, 3, 4, 5, 6, 7, 8), 5 -> Seq(1, 2, 4, 5, 7, 8),
      6 -> Seq(3, 4, 6, 7),       7 -> Seq(3, 4, 5, 6, 7, 8),          8 -> Seq(4, 5, 7, 8)
    ),
  )
