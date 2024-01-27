package it.unibo.distributedfrp.frp.timer

import it.unibo.distributedfrp.AbstractTest
import it.unibo.distributedfrp.frp.timer.*
import it.unibo.distributedfrp.test.utils.time.StopWatch
import it.unibo.distributedfrp.utils.Clock

import java.util.concurrent.{Executors, ScheduledExecutorService}
import scala.concurrent.duration.*

/** Test for [[TimerFactory]]. */
class TimerFactoryTest extends AbstractTest:
  private given clock: Clock = Clock.SystemClock
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
    scheduler.awaitTermination(Defaults.timeout.length, Defaults.timeout.unit)
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
    scheduler.awaitTermination(Defaults.timeout.length, Defaults.timeout.unit)
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
    scheduler.awaitTermination(Defaults.timeout.length, Defaults.timeout.unit)
    stopWatch.laps should have size 2
    stopWatch.laps.head should be >= timer.duration
    stopWatch.laps(1) should be >= (resetTime + timer.duration)
  }
