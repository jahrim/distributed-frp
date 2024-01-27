package it.unibo.distributedfrp.test.utils.executor

import it.unibo.distributedfrp.AbstractTest

import java.util.concurrent.{Executors, Semaphore}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

/** Test for [[ExecutionContextSpy]]. */
class ExecutionContextSpyTest extends AbstractTest:
  private val nThreads: Int = Runtime.getRuntime.availableProcessors() + 1
  private val executor: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(nThreads))

  "An ExecutionContextSpy" should "have no busy threads when created" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    spy.busyThreads shouldEqual 0
  }

  it should "keep track of the number of threads that are currently busy executing some tasks" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    given ExecutionContext = spy
    val semaphore: Semaphore = Semaphore(0)
    val tasksCompletion: Future[?] =
      Future.sequence(
        Seq.range(0, nThreads)
          .map(taskId => {
            val (ready, completion) = (Promise[Unit](), Promise[Unit]())
            spy.execute(() => { ready.success(()); semaphore.acquire(); completion.success(()) })
            Await.ready(ready.future, Defaults.timeout)
            spy.busyThreads shouldEqual taskId + 1
            completion.future
          })
      )
    semaphore.release(nThreads)
    Await.ready(tasksCompletion, Defaults.timeout)
    spy.busyThreads shouldEqual 0
  }

  it should "have a null maximum potential concurrency when created" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    spy.maxConcurrency shouldEqual 0
  }

  it should "keep track of the maximum number of busy threads ever recorded" in {
    val spy: ExecutionContextSpy = ExecutionContextSpy(executor)
    given ExecutionContext = spy
    val semaphore: Semaphore = Semaphore(0)
    val tasksCompletion: Future[?] =
      Future.sequence(
        Seq.range(0, nThreads)
          .map(taskId => {
            val (ready, completion) = (Promise[Unit](), Promise[Unit]())
            spy.execute(() => { ready.success(()); semaphore.acquire(); completion.success(()) })
            Await.ready(ready.future, Defaults.timeout)
            spy.maxConcurrency shouldEqual taskId + 1
            completion.future
          })
      )
    semaphore.release(nThreads)
    Await.ready(tasksCompletion, Defaults.timeout)
    spy.maxConcurrency shouldEqual nThreads
  }
