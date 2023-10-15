package it.unibo.distributedfrp.utils

import it.unibo.distributedfrp.utils.mock.MockClock
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.concurrent.duration.*

/** Test for [[StopWatch]]. */
class StopWatchTest extends AnyFlatSpec with should.Matchers with BeforeAndAfter:
  private given clock: MockClock = MockClock()

  /**
   * Forwards the time of the specified [[FiniteDuration]], simulating
   * the execution of a task.
   *
   * @param duration the specified [[FiniteDuration]].
   */
  private def doSomethingFor(duration: FiniteDuration): Unit = clock.forwardTime(duration)

  "A Stopwatch" should "let the user measure the execution time of a task" in {
    val stopWatch: StopWatch = StopWatch.basic.start()
    doSomethingFor(200.milliseconds)
    stopWatch.lap() shouldEqual 200.milliseconds
  }

  it should "let the user keep track of the time elapsed since it was started" in {
    val stopWatch: StopWatch = StopWatch.basic.start()
    doSomethingFor(200.milliseconds)
    stopWatch.lap()
    doSomethingFor(200.milliseconds)
    stopWatch.lap()
    doSomethingFor(500.milliseconds)
    stopWatch.lap()
    stopWatch.laps shouldEqual Seq(200, 400, 900).map(_.milliseconds)
  }

  it should "let the user pause the tracking of time for a while" in {
    val stopWatch: StopWatch = StopWatch.basic.start()
    doSomethingFor(200.milliseconds)
    stopWatch.pause()
    doSomethingFor(200.milliseconds)
    stopWatch.start()
    doSomethingFor(500.milliseconds)
    stopWatch.lap() shouldEqual 700.milliseconds
  }

  it should "let the user reset the tracking of time" in {
    val stopWatch: StopWatch = StopWatch.basic.start()
    doSomethingFor(200.milliseconds)
    stopWatch.reset()
    doSomethingFor(200.milliseconds)
    stopWatch.start()
    doSomethingFor(500.milliseconds)
    stopWatch.lap() shouldEqual 500.milliseconds
  }

  it should "let the user reset the registered laps" in {
    val stopWatch: StopWatch = StopWatch.basic.start()
    doSomethingFor(200.milliseconds)
    stopWatch.lap()
    doSomethingFor(200.milliseconds)
    stopWatch.lap()
    doSomethingFor(500.milliseconds)
    stopWatch.lap()
    stopWatch.laps shouldEqual Seq(200, 400, 900).map(_.milliseconds)
    stopWatch.reset().laps shouldEqual Seq()
  }

  it should "throw an error when pausing if it was not started" in {
    assertThrows[IllegalStateException](StopWatch.basic.pause())
  }

  it should "throw an error when tracking time if it was not started" in {
    assertThrows[IllegalStateException](StopWatch.basic.lap())
  }
