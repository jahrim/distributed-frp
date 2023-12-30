package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.{AbstractTest, utils}

import scala.concurrent.ExecutionContext

/** An [[AbstractTest]] specific for the FRASP framework. */
trait FraspTest extends AbstractTest:
  protected given ExecutionContext = ExecutionContext.global
  export utils.Liftable.*
  export utils.{LowerBounded, UpperBounded}

  /** The id of a device that does not exist. */
  protected def noId: Int = Int.MinValue
