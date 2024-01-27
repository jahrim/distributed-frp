package it.unibo.distributedfrp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfter, Inspectors}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

/** Tests in this suite should extend this trait. */
trait AbstractTest extends AnyFlatSpec with Matchers with BeforeAndAfter with Inspectors:
  /**
   * @param name the specified name.
   * @return a symbol with the specified name.
   */
  protected def symbol(name: String): String = s"`$name`"

  extension (self: Double){
    /**
     * @param precision the specified number of decimals.
     * @return the [[Double]] with the specified number of decimals that
     *         is closest to this [[Double]]. If this [[Double]] is not
     *         finite, return this [[Double]] unchanged.
     */
    def round(precision: Int): Double = self match
      case d if d.isFinite => BigDecimal(self).setScale(precision, BigDecimal.RoundingMode.HALF_UP).toDouble
      case _ => self
  }

  /** A collection of default constants to be used within this test. */
  object Defaults:
    /** The default [[ExecutionContext]] for asynchronous computation in this test. */
    given executor: ExecutionContext = ExecutionContext.global

    /** The default timeout for blocking code in this test. */
    val timeout: FiniteDuration = 60.seconds

    /** The default [[Double]] precision in this test. */
    val precision: Int = 2
