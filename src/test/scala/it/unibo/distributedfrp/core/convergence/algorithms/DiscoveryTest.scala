package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest

/** A [[ConvergenceTest]] for the network discovery algorithm. */
class DiscoveryTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val Discovery = symbol("discovery")

  import DefaultSimulator.incarnation.{*, given}

  Discovery should "collect the identifiers of all the devices in a network" in convergenceTest(
    simulator = DefaultSimulator,
    flow = discovery,
    limit = Seq.range(0, environment.nDevices).map(_ -> Set.range(0, environment.nDevices)).toMap
  )
