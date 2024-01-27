package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest
import it.unibo.distributedfrp.simulation.environment.{Environment, EnvironmentWithTags}

/** A [[ConvergenceTest]] for the round tracking algorithm. */
class ZipWithRoundTest extends ConvergenceTest.Defaults.WithStepSimulator:
  private val ZipWithRound = symbol("zipWithRound")

  import DefaultSimulator.incarnation.{Environment as _, *, given}
  override protected def defaultEnvironment: EnvironmentWithTags =
    EnvironmentWithTags(Environment.euclideanGrid(5, 5))

  ZipWithRound should
    "zip the computations of each device in the network with the round when it was computed, " +
    "considering each round to happen for a device when the computation of the device changes" in convergenceTest(
    simulator = DefaultSimulator,
    flow = zipWithRound(sequence(0, 0, 1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10, 10)),
    limit = Seq.range(0, environment.nDevices).map(_ -> (10, 10)).toMap
  )
