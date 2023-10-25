package it.unibo.distributedfrp.test.utils.mock.clock

import it.unibo.distributedfrp.test.utils.AbstractTest
import it.unibo.distributedfrp.test.utils.mock.clock.MockClockScheduler
import it.unibo.distributedfrp.test.utils.mock.clock.MockClockScheduler.PendingTask
import it.unibo.distributedfrp.test.utils.time.StopWatch

import scala.concurrent.duration.*

/** Test for [[MockClockScheduler]]. */
class MockClockSchedulerTest extends AbstractTest:
  private var scheduler: MockClockScheduler = MockClockScheduler()
  private var stopWatch: StopWatch = StopWatch.basic(using scheduler)

  before {
    this.scheduler = MockClockScheduler()
    this.stopWatch = StopWatch.basic(using scheduler).start()
  }

  "A MockClockScheduler" should "start at the origin of its timeline" in {
    scheduler.nanos() shouldEqual MockClockScheduler.Origin.toNanos
    scheduler.time shouldEqual MockClockScheduler.Origin
  }

  it should "let the user schedule tasks at different points in its timeline" in {
    stopWatch.lap()
    scheduler.scheduleAt(100.milliseconds){ stopWatch.lap() }
    scheduler.scheduleAt(200.milliseconds){ stopWatch.lap() }
    scheduler.scheduleAt(50.milliseconds){ stopWatch.lap() }
    scheduler.executePending()
    stopWatch.laps shouldEqual Seq(0, 50, 100, 200).map(_.milliseconds)
  }

  it should "let the user schedule tasks within other scheduled tasks" in {
    stopWatch.lap()
    scheduler.scheduleAt(100.milliseconds){
      stopWatch.lap()
      scheduler.scheduleAt(50.milliseconds){ stopWatch.lap() }
      scheduler.scheduleAt(200.milliseconds){ stopWatch.lap() }
    }
    scheduler.executePending()
    stopWatch.laps shouldEqual Seq(0, 100, 50, 200).map(_.milliseconds)
  }

  it should "let the user schedule tasks within other scheduled tasks some time after their execution" in {
    stopWatch.lap()
    scheduler.scheduleAt(100.milliseconds) {
      stopWatch.lap()
      scheduler.scheduleAfter(50.milliseconds) { stopWatch.lap() }
      scheduler.scheduleAfter(200.milliseconds) { stopWatch.lap() }
    }
    scheduler.executePending()
    stopWatch.laps shouldEqual Seq(0, 100, 150, 300).map(_.milliseconds)
  }

  it should "let the user cancel the execution of scheduled tasks" in {
    val pendingTask: PendingTask[?] = scheduler.scheduleAt(100.milliseconds){}
    scheduler.cancel(pendingTask)
    scheduler.executePending()
    pendingTask.execution.isCompleted shouldBe false
  }

  it should "let the user cancel the execution of scheduled tasks within other scheduled tasks" in {
    val pendingTask: PendingTask[?] = scheduler.scheduleAt(100.milliseconds) {}
    scheduler.scheduleAt(50.milliseconds) { scheduler.cancel(pendingTask) }
    scheduler.executePending()
    pendingTask.execution.isCompleted shouldBe false
  }

  it should "not track the time outside of its scheduled tasks" in {
    scheduler.scheduleAt(100.milliseconds){}
    scheduler.executePending()
    scheduler.time shouldEqual MockClockScheduler.Origin
  }
