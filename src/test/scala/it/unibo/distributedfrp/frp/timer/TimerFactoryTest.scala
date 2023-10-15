package it.unibo.distributedfrp.frp.timer

import it.unibo.distributedfrp.frp.timer.*
import it.unibo.distributedfrp.utils.{Clock, StopWatch}
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.util.concurrent.{Executors, ScheduledExecutorService}
import scala.concurrent.duration.*

/** Test for [[TimerFactory]]. */
class TimerFactoryTest extends AnyFlatSpec with should.Matchers with BeforeAndAfter:
  private given clock: Clock = Clock.SystemClock
  private val timeout: FiniteDuration = 30.seconds
  private var stopWatch: StopWatch = StopWatch.basic

  before { this.stopWatch = StopWatch.basic.start() }

  "A TimerFactory" should "create timers whose duration can be accessed" in {
    val timerFactory: TimerFactory[Timer] = TimerFactory.async
    val timer: Timer = timerFactory.create(100.milliseconds)
    timer.duration shouldEqual 100.milliseconds
  }

  it should "create timers ticking after a certain duration since their creation" in {
    given scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor()
    val timerFactory: TimerFactory[Timer] = TimerFactory.async
    val timer: Timer = timerFactory.create(100.milliseconds)
    timer.ticks.listen(_ => stopWatch.lap())
    scheduler.shutdown()
    scheduler.awaitTermination(timeout.length, timeout.unit)
    stopWatch.laps should have size 1
    stopWatch.laps.head should be >= timer.duration
  }

  it should "create timers that can be reset before ticking" in {
    given scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor()
    val timerFactory: TimerFactory[Timer] = TimerFactory.async
    val timer: Timer = timerFactory.create(100.milliseconds)
    val resetTime: FiniteDuration = 50.milliseconds
    timer.ticks.listen(_ => stopWatch.lap())
    scheduler.schedule[Unit](() => { timer.reset(); scheduler.shutdown() }, resetTime.length, resetTime.unit)
    scheduler.awaitTermination(timeout.length, timeout.unit)
    stopWatch.laps should have size 1
    stopWatch.laps.head should be >= (resetTime + timer.duration)
  }

  it should "create timers that can be reset after ticking" in {
    given scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor()
    val timerFactory: TimerFactory[Timer] = TimerFactory.async
    val timer: Timer = timerFactory.create(100.milliseconds)
    val resetTime: FiniteDuration = 200.milliseconds
    timer.ticks.listen(_ => stopWatch.lap())
    scheduler.schedule[Unit](() => { timer.reset(); scheduler.shutdown() }, resetTime.length, resetTime.unit)
    scheduler.awaitTermination(timeout.length, timeout.unit)
    stopWatch.laps should have size 2
    stopWatch.laps.head should be >= timer.duration
    stopWatch.laps(1) should be >= (resetTime + timer.duration)
  }
