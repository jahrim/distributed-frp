package it.unibo.distributedfrp.utils

import scala.concurrent.{ExecutionContext, Future}

/**
 * A scheduler accepting resources from producers and deciding
 * the order by which those resources will be provided to consumers.
 *
 * @tparam R the type of resources managed by the scheduler.
 */
trait Scheduler[R]:
  /**
   * Accept the specified resource to be consumed in the future.
   *
   * @param resource the specified resource.
   * @return a succeeded [[Future]] completing when the specified resource
   *         has been consumed.
   */
  def schedule(resource: R): Future[Unit]

  /**
   * Consume the next resource in the order decided by this [[Scheduler]].
   *
   * @param consumer the consumer of the next resource.
   * @param executor the [[ExecutionContext]] where the consumer will be run.
   *                 Defaults to the caller of [[next]].
   * @tparam A the type of result produced by the consumer.
   * @return a succeeded [[Future]] containing the result produced by the
   *         consumer if the consumer was successful; a failed [[Future]]
   *         otherwise.
   */
  def next[A](consumer: Option[R] => A)(using executor: ExecutionContext = ExecutionContext.parasitic): Future[A]
