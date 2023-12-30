package it.unibo.distributedfrp.core.convergence

/** A [[ConvergenceTest]] for the share construct. */
class ShareTest extends ConvergenceTest.WithDefaults:
  private val Share = symbol("share")

  import defaultSimulator.incarnation.{*, given}

  Share should "..." in {}

  // TODO implement tests following `share` specifications
