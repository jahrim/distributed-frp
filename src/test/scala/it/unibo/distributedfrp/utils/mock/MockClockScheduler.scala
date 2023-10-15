package it.unibo.distributedfrp.utils.mock

import it.unibo.distributedfrp.utils.Clock
import it.unibo.distributedfrp.utils.mock.MockClockScheduler.*
import it.unibo.distributedfrp.utils.mock.{MockClock, MockClockScheduler}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
 * A [[Clock]] that can schedule tasks within its timeline.
 *
 * In particular, the time of this [[Clock]] changes only within
 * the scheduled tasks, meaning that the [[nanos]] and [[time]]
 * methods make sense only when called within a task.
 *
 * Each task will be executed at the exact point in the timeline
 * it was scheduled to be executed. Then, it can schedule new tasks
 * in other absolute or relative points in the timeline.
 *
 * The tasks are executed by the caller of the [[executePending]]
 * method.
 */
class MockClockScheduler() extends Clock:
  private val _clock: MockClock = MockClock()
  private val _taskManager: TaskManager = TaskManager()
  private val _taskIdGenerator: TaskIdGenerator = TaskIdGenerator()

  /**
   * @return the number of nanoseconds elapsed since the origin of the
   *         timeline tracked by this [[Clock]].
   * @note for a [[MockClockScheduler]], outside of a [[PendingTask PendingTask]]
   *       it will always return the origin of the timeline of this [[Clock]].
   */
  override def nanos(): Long = this._clock.nanos()
  /**
   * @return the [[FiniteDuration]] elapsed since the origin of the
   *         timeline tracked by this [[Clock]].
   * @note for a [[MockClockScheduler]], outside of a [[PendingTask PendingTask]]
   *       it will always return the origin of the timeline of this [[Clock]].
   */
  override def time: FiniteDuration = this._clock.time

  /**
   * Execute all [[PendingTask PendingTask]]s at the respective
   * times they were scheduled for execution.
   *
   * The [[PendingTask PendingTask]]s are executed in order in
   * the timeline and in order of registration.
   *
   * @note this method allows [[PendingTask PendingTask]]s to
   *       register or cancel other [[PendingTask PendingTask]]s
   *       within the scheduled [[PendingTask PendingTask]]s as
   *       they are executed. In particular, it is allowed to
   *       register new [[PendingTask PendingTask]]s even in the
   *       past or in the present and they will still be executed
   *       in the proper order, after the execution of the current
   *       [[PendingTask PendingTask]].
   */
  def executePending(): Unit =
    @tailrec def executeNext(): Unit =
      this._taskManager.pop() match
        case Some(runningTask) =>
          this._clock.setTime(runningTask.scheduledAt)
          runningTask()
          executeNext()
        case None =>
    executeNext()
    this._clock.setTime(MockClockScheduler.Origin)

  /**
   * Append the specified task to be executed at the
   * specified time in the timeline tracked by this
   * [[MockClockScheduler]].
   *
   * @param time the specified time.
   * @param task the specified task.
   * @tparam O the result type of the specified task.
   * @return a new [[PendingTask PendingTask]] that can be used to keep track
   *         of the state of the appended task.
   */
  def scheduleAt[O](time: FiniteDuration)(task: => O): PendingTask[O] =
    this._taskManager.register(PendingTask(this._taskIdGenerator.next(), time, () => task))

  /** As [[scheduleAt scheduleAt(this.time + duration)(task)]]. */
  def scheduleAfter[O](duration: FiniteDuration)(task: => O): PendingTask[O] =
    this.scheduleAt(this.time + duration)(task)

  /**
   * Remove the specified [[PendingTask PendingTask]] from the
   * [[PendingTask PendingTask]]s appended to this [[MockClockScheduler]].
   *
   * @param task the specified [[PendingTask PendingTask]].
   */
  def cancel(task: PendingTask[?]): Unit =
    this._taskManager.cancel(task)

/** Companion object of [[MockClockScheduler]]. */
object MockClockScheduler:
  /** The default origin of time for the timelines tracked by [[MockClockScheduler]]s. */
  val Origin: FiniteDuration = MockClock.Origin

  /** The identifier of a [[PendingTask]]. */
  type TaskId = Long

  /**
   * A task that needs to be executed by a [[MockClockScheduler]].
   *
   * @param id          the unique identifier of this [[PendingTask PendingTask]]
   *                    within the owner [[MockClockScheduler]].
   * @param scheduledAt the point in the timeline of the owner [[MockClockScheduler]]
   *                    when this [[PendingTask PendingTask]] will be executed.
   * @param taskBody    the body of the [[PendingTask PendingTask]].
   * @tparam O the return type of the [[PendingTask PendingTask]].
   */
  case class PendingTask[O](
    private[MockClockScheduler] val id: TaskId,
    private[MockClockScheduler] val scheduledAt: FiniteDuration,
    private val taskBody: () => O
  ):
    private val _execution: Promise[O] = Promise()

    /**
     * @return a [[Future]] completing with the result of this [[PendingTask PendingTask]]
     *         when its execution is completed.
     */
    def execution: Future[O] = this._execution.future

    /** Run this [[PendingTask PendingTask]]. */
    private[MockClockScheduler] def apply(): Unit = this._execution.complete(Try(taskBody()))

  /** A thread-safe manager for [[PendingTask PendingTask]]s. */
  private case class TaskManager():
    /**
     * A given [[Ordering]] for [[PendingTask PendingTask]]s that
     * prioritizes [[PendingTask PendingTask]]s that are to be
     * executed earlier in the timeline and with a lower identifier.
     */
    private given taskPriority: Ordering[PendingTask[?]] =
      (x, y) => 2 * (y.scheduledAt compare x.scheduledAt) + (y.id compare x.id)

    import scala.collection.mutable
    private val _pendingTasks: mutable.PriorityQueue[PendingTask[?]] = mutable.PriorityQueue()

    /**
     * Register the specified [[PendingTask PendingTask]].
     *
     * @param task the specified [[PendingTask PendingTask]].
     * @return the specified [[PendingTask PendingTask]] itself.
     */
    def register[O](task: PendingTask[O]): PendingTask[O] =
      synchronized(this._pendingTasks.enqueue(task))
      task

    /**
     * Remove and return the next [[PendingTask PendingTask]] to be executed, if any.
     *
     * @return an [[Option]] containing the next [[PendingTask PendingTask]] to be
     *         executed if present; an empty [[Option]] otherwise.
     */
    def pop(): Option[PendingTask[?]] =
      Try(synchronized(this._pendingTasks.dequeue())).toOption

    /**
     * Remove the specified [[PendingTask PendingTask]] from the tasks to be
     * executed.
     */
    def cancel(task: PendingTask[?]): Unit =
      synchronized(this._pendingTasks.enqueue(this._pendingTasks.dequeueAll.filter(_.id != task.id)*))

  /** A thread-safe unlimited [[Iterator]] of unique [[TaskId]]s. */
  private case class TaskIdGenerator() extends Iterator[TaskId]:
    private val _taskIds: Iterator[TaskId] = LazyList.iterate(0L)(_ + 1).iterator
    override def next(): TaskId = synchronized(this._taskIds.next())
    override def hasNext: Boolean = true
