package it.unibo.distributedfrp.test.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfter, Inspectors}

/** Tests in this suite should extend this trait. */
trait AbstractTest extends AnyFlatSpec with Matchers with BeforeAndAfter with Inspectors:
  /**
   * @param name the specified name.
   * @return a symbol with the specified name.
   */
  def symbol(name: String): String = s"`$name`"
