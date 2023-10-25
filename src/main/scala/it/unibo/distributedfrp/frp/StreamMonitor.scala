package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.StreamExtension.*
import nz.sodium

/**
 * A monitor that records the events of a [[Stream Stream]].
 *
 * @tparam A the type of events recorded by this [[StreamMonitor]].
 * @tparam S the type of [[Stream Stream]] monitored by this [[StreamMonitor]].
 */
trait StreamMonitor[A, S <: Stream[A]]:
  /** @return the [[Stream Stream]] being monitored by this [[StreamMonitor]]. */
  def stream: S
  /**
   * @return a sequence of all the events recorded by this [[StreamMonitor]]
   *         in the order they were observed.
   */
  def eventLog: Seq[A]

/** Companion object of [[StreamMonitor]]. */
object StreamMonitor:
  /**
   * @param stream the specified [[Stream Stream]].
   * @param memory the memory of the [[StreamMonitor]] (default: unlimited). The memory
   *               indicates the number of events the [[StreamMonitor]] will keep track
   *               of before discarding the oldest ones.
   * @tparam A the type of events produced by the specified [[Stream Stream]].
   * @tparam S the type of the specified [[Stream Stream]].
   * @return a new [[StreamMonitor]] monitoring the specified [[Stream Stream]].
   */
  def apply[A, S <: Stream[A]](stream: S, memory: Int = Int.MaxValue): StreamMonitor[A, S] =
    BasicStreamMonitor(stream, memory)

  /**
   * A monitor that records the events of a [[Stream Stream]].
   *
   * @param stream the [[Stream Stream]] being monitored by this [[StreamMonitor]].
   * @param memory the memory of the [[StreamMonitor]] (default: unlimited). The memory
   *               indicates the number of events the new [[StreamMonitor]] will keep track
   *               of before discarding the oldest ones.
   * @tparam A the type of events produced by the specified [[Stream Stream]].
   * @tparam S the type of the specified [[Stream Stream]].
   */
  private case class BasicStreamMonitor[A, S <: Stream[A]](
    override val stream: S,
    memory: Int = Int.MaxValue
  ) extends StreamMonitor[A, S]:
    private val _latestEvents: sodium.Cell[Seq[A]] = stream.cold(memory).holdLazy(sodium.Lazy(Seq.empty))
    override def eventLog: Seq[A] = this._latestEvents.sample()
