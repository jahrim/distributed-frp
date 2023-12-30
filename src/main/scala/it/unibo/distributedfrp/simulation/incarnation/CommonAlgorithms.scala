package it.unibo.distributedfrp.simulation.incarnation

import it.unibo.distributedfrp.utils.Liftable.*
import it.unibo.distributedfrp.utils.UpperBounded

/**
 * A [[SimulationIncarnation]] with [[CommonSensors]] extended with
 * the most common collective computing algorithms.
 */
trait CommonAlgorithms:
  self: SimulationIncarnation with CommonSensors =>

  /**
   * A policy that decides what message to keep when receiving
   * multiple messages from equidistant neighbors during [[gradcast]].
   */
  @FunctionalInterface
  trait GradcastMessagePriorityPolicy[M] extends UpperBounded[(Double, Option[M])]:
    /**
     * @param x the first specified message.
     * @param y the second specified message.
     * @return the priority difference between the first and the second
     *         specified messages.
     */
    def relativePriority(x: M, y: M): Int

    override final def upperBound: (Double, Option[M]) =
      (Double.MaxValue, None)

    override final def compare(x: (Double, Option[M]), y: (Double, Option[M])): Int =
      2 * summon[Ordering[Double]].compare(x._1, y._1) +
        1 * compareMessages(x._2, y._2)

    private def compareMessages(x: Option[M], y: Option[M]): Int =
      if x.isEmpty || y.isEmpty then
        if x.isDefined then -1 else 1
      else
        relativePriority(y.get, x.get)

  /** Companion object of [[GradcastMessagePriorityPolicy GradcastMessagePriorityPolicy]]. */
  object GradcastMessagePriorityPolicy:
    /** A [[GradcastMessagePriorityPolicy GradcastMessagePriorityPolicy]] that keeps the first message. */
    def first[M]: GradcastMessagePriorityPolicy[M] = (x, y) => 1

    /** A [[GradcastMessagePriorityPolicy GradcastMessagePriorityPolicy]] that keeps the last message. */
    def last[M]: GradcastMessagePriorityPolicy[M] = (x, y) => -1

    /** A [[GradcastMessagePriorityPolicy GradcastMessagePriorityPolicy]] that keeps the highest message. */
    def highest[M: Ordering]: GradcastMessagePriorityPolicy[M] = summon[Ordering[M]].compare

    /** A [[GradcastMessagePriorityPolicy GradcastMessagePriorityPolicy]] that keeps the lowest message. */
    def lowest[M: Ordering]: GradcastMessagePriorityPolicy[M] = -highest.relativePriority(_, _)

  /**
   * Compute the minimum network distance from any of the specified
   * sources for each device in the network.
   *
   * A device that cannot reach any sources computes a distance equals
   * to [[Double.PositiveInfinity]].
   *
   * @param sources the specified sources.
   */
  def gradient(sources: Flow[Boolean]): Flow[Double] =
    loop(Double.PositiveInfinity) { distanceFromSource =>
      mux(sources) {
        constant(0.0)
      } {
        liftTwice(nbrRange, nbr(distanceFromSource))(_ + _).withoutSelf.min
      }
    }

  /**
   * Compute the messages produced by the closest of the specified sources
   * for each device in the network, using [[gradient]].
   *
   * A device that cannot reach any sources computes a message equals to [[None]].
   *
   * @param sources  the specified sources.
   * @param messages the messages produced by the specified sources.
   */
  def gradcast[M](sources: Flow[Boolean], messages: Flow[M])(using
    priorityPolicy: GradcastMessagePriorityPolicy[M] = GradcastMessagePriorityPolicy.first[M]
  ): Flow[Option[M]] =
    loop((Double.PositiveInfinity, Option.empty[M])) { distanceAndMessageFromSource =>
      mux(sources) {
        messages.map(message => 0.0 -> Some(message))
      } {
        liftTwice(nbrRange, nbr(distanceAndMessageFromSource)) {
          case (nbrDistance, (nbrSourceDistance, message)) => (nbrDistance + nbrSourceDistance, message)
        }.withoutSelf.min
      }
    }.map(_._2)

  /**
   * Compute the minimum network distance from any of the specified
   * sources to any of the specified destinations for each device
   * in the network.
   *
   * If no source can reach any of the destinations, each device in
   * the network computes [[None]].
   *
   * @param sources      the specified sources.
   * @param destinations the specified destinations.
   */
  def minBlockDistance(sources: Flow[Boolean], destinations: Flow[Boolean]): Flow[Option[Double]] =
    gradcast(destinations, gradient(sources))(using GradcastMessagePriorityPolicy.lowest)

  /**
   * Compute the minimum network path from any of the specified
   * sources to any of the specified destinations.
   *
   * Each device in the network individually computes true, if
   * it belongs to the path, false otherwise.
   *
   * @param sources      the specified sources.
   * @param destinations the specified destinations.
   */
  def minChannel(sources: Flow[Boolean], destinations: Flow[Boolean]): Flow[Boolean] =
    lift(
      gradient(sources),
      gradient(destinations),
      minBlockDistance(sources, destinations).map(_.getOrElse(Double.MinValue))
    )(_ + _ <= _)

  /**
   * As [[minChannel minChannel(sources, destinations)]], but
   * it computes all the paths that deviate from the minimum
   * network path up to the specified maximum deviation,
   * including the minimum network path.
   *
   * @param sources      the specified sources.
   * @param destinations the specified destinations.
   * @param maxDeviation the specified maximum deviation.
   */
  def minRedundantChannel(
    sources: Flow[Boolean],
    destinations: Flow[Boolean],
    maxDeviation: Flow[Double]
  ): Flow[Boolean] =
    lift(gradient(minChannel(sources, destinations)), maxDeviation)(_ <= _)
