package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest
import it.unibo.distributedfrp.simulation.environment.Environment.euclideanGrid
import it.unibo.distributedfrp.simulation.environment.{EnvironmentFactory, EnvironmentWithTags}

/** A [[ConvergenceTest]] for the network discovery algorithm. */
class DiscoveryTest extends ConvergenceTest.WithDefaults:
  private val Discovery = symbol("discovery")

  import defaultSimulator.incarnation.{*, given}
  private given EnvironmentFactory[EnvironmentWithTags] = () => EnvironmentWithTags(euclideanGrid(cols = 5, rows = 5))

  private def discovery: Flow[Set[DeviceId]] =
    loop(Set.empty[DeviceId]) { knownDevices =>
      liftTwice(nbr(knownDevices), nbr(mid))(_ + _).map(_.values.foldLeft(Set.empty[DeviceId])(_ ++ _))
    }

  Discovery should "collect the identifiers of all the devices in a network" in convergenceTest(
    simulator = defaultSimulator,
    flow = discovery,
    limit = Seq.range(0, 25).map(_ -> Set.range(0, 25)).toMap
  )
