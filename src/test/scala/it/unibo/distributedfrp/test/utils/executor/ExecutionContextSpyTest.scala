package it.unibo.distributedfrp.test.utils.executor

import it.unibo.distributedfrp.AbstractTest

import java.util.concurrent.{Executors, Semaphore}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

/** Test for [[ExecutionContextSpy]]. */
class ExecutionContextSpyTest extends AbstractTest:
  private val nThreads: Int = Runtime.getRuntime.availableProcessors() + 1
  private val executor: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(nThreads))

  "An ExecutionContextSpy" should "have no pending tasks when created" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    spy.pendingTasks shouldEqual 0
  }

  it should "keep track of the number of pending tasks" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    val semaphore: Semaphore = Semaphore(0)
    Seq.range(0, nThreads)
      .map(taskId =>
        spy.execute(() => semaphore.acquire())
        spy.pendingTasks shouldEqual taskId + 1
      )
    semaphore.release(nThreads)
    spy.awaitTaskCompletion()
    spy.pendingTasks shouldEqual 0
  }

  it should "have no busy threads when created" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    spy.busyThreads shouldEqual 0
  }

  it should "keep track of the number of threads that are currently busy executing some tasks" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    val semaphore: Semaphore = Semaphore(0)
    Seq.range(0, nThreads)
      .map(taskId =>
        val ready = Promise[Unit]()
        spy.execute(() => { ready.success(()); semaphore.acquire() })
        Await.ready(ready.future, Defaults.timeout)
        spy.busyThreads shouldEqual taskId + 1
      )
    semaphore.release(nThreads)
    spy.awaitTaskCompletion()
    spy.busyThreads shouldEqual 0
  }

  it should "have a null maximum potential concurrency when created" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    spy.maxConcurrency shouldEqual 0
  }

  it should "keep track of the maximum number of busy threads ever recorded" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    val semaphore: Semaphore = Semaphore(0)
    Seq.range(0, nThreads)
      .map(taskId =>
        val ready = Promise[Unit]()
        spy.execute(() => { ready.success(()); semaphore.acquire() })
        Await.ready(ready.future, Defaults.timeout)
        spy.maxConcurrency shouldEqual taskId + 1
      )
    semaphore.release(nThreads)
    spy.awaitTaskCompletion()
    spy.maxConcurrency shouldEqual nThreads
  }
