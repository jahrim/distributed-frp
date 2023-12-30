package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest
import it.unibo.distributedfrp.simulation.environment.Environment.euclideanGrid
import it.unibo.distributedfrp.simulation.environment.{EnvironmentFactory, EnvironmentWithTags}

/** A [[ConvergenceTest]] for the leader election algorithm. */
class LeaderElectionTest extends ConvergenceTest.WithDefaults:
  private val LeaderElection = symbol("leaderElection")
  
  import defaultSimulator.incarnation.{*, given}
  private given EnvironmentFactory[EnvironmentWithTags] = () => EnvironmentWithTags(euclideanGrid(5, 5))

  private def leaderElection: Flow[DeviceId] =
    loop(Int.MinValue) { leaderId => lift(mid, nbr(leaderId).max)(math.max) }

  LeaderElection should "elect a device as the leader of a network" in convergenceTest(
    simulator = defaultSimulator,
    flow = leaderElection,
    limit = Seq.range(0, 25).map(_ -> 24).toMap
  )
  it should "elect different leaders in different non-overlapping subnetworks" in convergenceTest(
    simulator = defaultSimulator,
    flow = branch(mid.map(_ < 10)) { leaderElection } { leaderElection },
    limit = Map(
      0 -> 9,   1 -> 9,   2 -> 9,   3 -> 9,   4 -> 9,
      5 -> 9,   6 -> 9,   7 -> 9,   8 -> 9,   9 -> 9,
      10 -> 24, 11 -> 24, 12 -> 24, 13 -> 24, 14 -> 24,
      15 -> 24, 16 -> 24, 17 -> 24, 18 -> 24, 19 -> 24,
      20 -> 24, 21 -> 24, 22 -> 24, 23 -> 24, 24 -> 24,
    )
  )
