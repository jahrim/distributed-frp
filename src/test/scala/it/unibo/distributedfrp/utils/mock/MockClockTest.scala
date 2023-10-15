package it.unibo.distributedfrp.utils.mock

import it.unibo.distributedfrp.utils.Symbols
import it.unibo.distributedfrp.utils.mock.MockClock
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.concurrent.duration.*

/** Test for [[MockClock]]. */
class MockClockTest extends AnyFlatSpec
  with should.Matchers
  with BeforeAndAfter
  with Symbols:
  private var clock: MockClock = MockClock()
  before { this.clock = MockClock() }

  "A MockClock" should "start at the origin of its timeline" in {
    clock.nanos() shouldEqual MockClock.Origin.toNanos
    clock.time shouldEqual MockClock.Origin
  }

  it should "let the user set the current time in its timeline" in {
    val now: FiniteDuration = 100.nanoseconds
    clock.setTime(now)
    clock.nanos() shouldEqual now.toNanos
    clock.time shouldEqual now
  }

  it should "let the user forward the current time in its timeline" in {
    val past: FiniteDuration = clock.time
    val shift: FiniteDuration = 200.milliseconds
    clock.forwardTime(shift)
    clock.nanos() shouldEqual (past + shift).toNanos
    clock.time shouldEqual (past + shift)
  }

  it should "track a timeline different from the timelines of other clocks" in {
    val otherClock: MockClock = MockClock()
    clock.setTime(100.nanoseconds)
    clock.nanos() shouldNot equal(otherClock.nanos())
    clock.time shouldNot equal(otherClock.time)
  }
