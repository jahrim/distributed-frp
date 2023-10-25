package it.unibo.distributedfrp.test.utils.mock.timer

import it.unibo.distributedfrp.frp.timer.*
import it.unibo.distributedfrp.test.utils.AbstractTest
import it.unibo.distributedfrp.test.utils.mock.timer.MockTimerFactory
import it.unibo.distributedfrp.test.utils.mock.clock.MockClockScheduler
import it.unibo.distributedfrp.test.utils.time.StopWatch

import scala.concurrent.duration.*

/** Test for [[MockTimerFactory]]. */
class MockTimerFactoryTest extends AbstractTest:
  private given scheduler: MockClockScheduler = MockClockScheduler()
  private var stopWatch: StopWatch = StopWatch.basic

  before { this.stopWatch = StopWatch.basic.start() }

  "A MockTimerFactory" should "create timers whose duration can be accessed" in {
    val mockTimerFactory: TimerFactory[Timer] = MockTimerFactory.basic
    val timer: Timer = mockTimerFactory.create(200.milliseconds)
    timer.duration shouldEqual 200.milliseconds
  }

  it should "create timers ticking after a certain duration since their creation" in {
    val mockTimerFactory: TimerFactory[Timer] = MockTimerFactory.basic
    val timer: Timer = mockTimerFactory.create(200.milliseconds)
    timer.ticks.listen(_ => stopWatch.lap())
    scheduler.executePending()
    stopWatch.laps shouldEqual Seq(timer.duration)
  }

  it should "create timers that can be reset before ticking" in {
    val mockTimerFactory: TimerFactory[Timer] = MockTimerFactory.basic
    val timer: Timer = mockTimerFactory.create(200.milliseconds)
    val resetTime: FiniteDuration = 100.milliseconds
    timer.ticks.listen(_ => stopWatch.lap())
    scheduler.scheduleAt(resetTime){ timer.reset() }
    scheduler.executePending()
    stopWatch.laps shouldEqual Seq(resetTime + timer.duration)
  }

  it should "create timers that can be reset after ticking" in {
    val mockTimerFactory: TimerFactory[Timer] = MockTimerFactory.basic
    val timer: Timer = mockTimerFactory.create(200.milliseconds)
    val resetTime: FiniteDuration = 300.milliseconds
    timer.ticks.listen(_ => stopWatch.lap())
    scheduler.scheduleAt(resetTime) { timer.reset() }
    scheduler.executePending()
    stopWatch.laps shouldEqual Seq(timer.duration, resetTime + timer.duration)
  }
