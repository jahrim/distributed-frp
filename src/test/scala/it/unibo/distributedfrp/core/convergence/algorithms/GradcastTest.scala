package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest
import it.unibo.distributedfrp.simulation.environment.Environment.euclideanGrid
import it.unibo.distributedfrp.simulation.environment.{EnvironmentFactory, EnvironmentWithTags}

/** A [[ConvergenceTest]] for the gradcast algorithm. */
class GradcastTest extends ConvergenceTest.WithDefaults:
  private val Gradcast = symbol("gradcast")

  import defaultSimulator.incarnation.{*, given}
  private given EnvironmentFactory[EnvironmentWithTags] = () => EnvironmentWithTags(euclideanGrid(5, 5))

  private val message: Some[String] = Some("message")
  Gradcast should "compute the message of the single source for each device" in convergenceTest(
    simulator = defaultSimulator,
    flow = {
      summon[Environment].tag(SourceTag, Set(0))
      gradcast(isSource, constant(message.value))
    },
    limit = Seq.range(0, 25).map(_ -> message).toMap
  )

  private def gradientPartition(centroids: Set[DeviceId])(using
    priorityPolicy: GradcastMessagePriorityPolicy[DeviceId] = GradcastMessagePriorityPolicy.lowest[DeviceId]
  ): Flow[Option[DeviceId]] =
    gradcast(mid.map(centroids), mid)

  it should "compute the message of the nearest source for each device" in convergenceTest(
    simulator = defaultSimulator,
    flow = gradientPartition(centroids = Set(0, 6, 24)),
    limit = Map(
       0 -> Some(0),  1 -> Some(0),  2 -> Some(6),   3 -> Some(6),   4 -> Some(6),
       5 -> Some(0),  6 -> Some(6),  7 -> Some(6),   8 -> Some(6),   9 -> Some(6),
      10 -> Some(6), 11 -> Some(6), 12 -> Some(6),  13 -> Some(6),  14 -> Some(24),
      15 -> Some(6), 16 -> Some(6), 17 -> Some(6),  18 -> Some(24), 19 -> Some(24),
      20 -> Some(6), 21 -> Some(6), 22 -> Some(24), 23 -> Some(24), 24 -> Some(24),
    )
  )
