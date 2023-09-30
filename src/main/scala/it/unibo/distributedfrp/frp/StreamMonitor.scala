package it.unibo.distributedfrp.frp

import nz.sodium
import StreamExtension.{*, given}

/**
 * A monitor that records the events of a [[sodium.Stream Stream]].
 *
 * @tparam A the type of events recorded by this [[StreamMonitor]].
 * @tparam S the type of [[sodium.Stream Stream]] monitored by this [[StreamMonitor]].
 */
trait StreamMonitor[A, S <: sodium.Stream[A]]:
  /** @return the [[sodium.Stream Stream]] being monitored by this [[StreamMonitor]]. */
  def stream: S
  /**
   * @return a sequence of all the events recorded by this [[StreamMonitor]]
   *         in the order they were observed.
   */
  def eventLog: Seq[A]

/** Companion object of [[StreamMonitor]]. */
object StreamMonitor:
  /**
   * @param stream the specified [[sodium.Stream Stream]].
   * @param memory the memory of the [[StreamMonitor]] (default: unlimited). The memory
   *               indicates the number of events the [[StreamMonitor]] will keep track
   *               of before discarding the oldest ones.
   * @tparam A the type of events produced by the specified [[sodium.Stream Stream]].
   * @tparam S the type of the specified [[sodium.Stream Stream]].
   * @return a new [[StreamMonitor]] monitoring the specified [[sodium.Stream Stream]].
   */
  def apply[A, S <: sodium.Stream[A]](stream: S, memory: Int = Int.MaxValue): StreamMonitor[A, S] =
    BasicStreamMonitor(stream, memory)

  /**
   * A monitor that records the events of a [[sodium.Stream Stream]].
   *
   * @param stream the [[sodium.Stream Stream]] being monitored by this [[StreamMonitor]].
   * @param memory the memory of the [[StreamMonitor]] (default: unlimited). The memory
   *               indicates the number of events the new [[StreamMonitor]] will keep track
   *               of before discarding the oldest ones.
   * @tparam A the type of events produced by the specified [[sodium.Stream Stream]].
   * @tparam S the type of the specified [[sodium.Stream Stream]].
   */
  private case class BasicStreamMonitor[A, S <: sodium.Stream[A]](
    override val stream: S,
    memory: Int = Int.MaxValue
  ) extends StreamMonitor[A, S]:
    private val _latestEvents: sodium.Cell[Seq[A]] = stream.cold(memory).holdLazy(sodium.Lazy(Seq.empty))
    override def eventLog: Seq[A] = this._latestEvents.sample()
