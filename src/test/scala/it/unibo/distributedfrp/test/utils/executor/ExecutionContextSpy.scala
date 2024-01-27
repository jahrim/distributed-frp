package it.unibo.distributedfrp.test.utils.executor

import scala.concurrent.ExecutionContext

/**
 * A wrapper for [[ExecutionContext]] that keeps track of
 * additional information about the state of the executor.
 */
trait ExecutionContextSpy extends ExecutionContext:
  /** @return the number of [[Thread Thread]]s that are currently executing a task. */
  def busyThreads: Long

  /**
   * @return the maximum number of [[Thread Thread]]s that have been executing tasks
   *         concurrently since the creation of this [[ExecutionContextSpy]].
   * @note this is a metric of `potential` concurrency, as it does not consider the
   *       possibility of blocking tasks.
   */
  def maxConcurrency: Int

/** Companion object of [[ExecutionContext]]. */
object ExecutionContextSpy:
  /**
   * @param executionContext the specified [[ExecutionContext]].
   * @return a new [[ExecutionContextSpy]] monitoring the specified [[ExecutionContext]].
   */
  def apply(executionContext: ExecutionContext): ExecutionContextSpy =
    BasicExecutionContextSpy(executionContext)

  /** Basic implementation of [[ExecutionContextSpy]]. */
  private case class BasicExecutionContextSpy(underlying: ExecutionContext) extends ExecutionContextSpy:
    private var _busyThreads: Set[Long] = Set()
    private var _maxConcurrency: Int = 0

    export underlying.reportFailure
    override def execute(runnable: Runnable): Unit =
      underlying.execute(() => { beforeExecute(); runnable.run(); afterExecute() })
    override def busyThreads: Long = synchronized { this._busyThreads.size }
    override def maxConcurrency: Int = synchronized { this._maxConcurrency }

    /** Callback executed before the execution of each task. */
    private def beforeExecute(): Unit = synchronized {
      this._busyThreads = this._busyThreads + Thread.currentThread().threadId()
      this._maxConcurrency = math.max(this._maxConcurrency, this._busyThreads.size)
    }

    /** Callback executed after the execution of each task. */
    private def afterExecute(): Unit = synchronized {
      this._busyThreads = this._busyThreads - Thread.currentThread().threadId()
    }
