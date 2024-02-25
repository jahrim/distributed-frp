package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.core.FraspSamples.*
import it.unibo.distributedfrp.simulation.incarnation.{CommonAlgorithms, CommonSensors, SimulationIncarnation}
import it.unibo.distributedfrp.utils.Liftable.*

/**
 * A mixin for providing a collection of samples of FRASP programs
 * to a [[SimulationIncarnation]].
 */
trait FraspSamples:
  self: SimulationIncarnation with CommonSensors with CommonAlgorithms =>

  /** Collect all the results of the specified [[Flow Flow]]. */
  def accumulate[T](flow: => Flow[T], memory: Int = Int.MaxValue, calm: Boolean = true): Flow[Seq[T]] =
    loop(Seq.empty) {
      lift(_, flow) {
        case (acc, next) if acc.lastOption.forall(!calm || _ != next) => (acc :+ next).takeRight(memory)
        case (acc, _) => acc
      }
    }

  /** Assign the roles of [[Source]] or [[Obstacle]] to some devices in the environment. */
  def assignSourceOrObstacleRoles: Flow[Source.type | Obstacle.type | None.type] =
    mux(isSource){ constant(Source) }{ mux(isObstacle) { constant(Obstacle) } { constant(None) } }

  /** Select one of the two specified values depending on the specified condition, using [[branch]]. */
  def branchConditionally[A](selector: DeviceId => Boolean, ifSelected: => A, otherwise: => A): Flow[A] =
    branch(mid.map(selector)) { constant(ifSelected) } { constant(otherwise) }

  /** Collect the neighbors of each device, masking the id of a device with a specified value. */
  def collectMaskedNeighbors(maskedId: DeviceId, mask: DeviceId): Flow[Set[DeviceId]] =
    nbr(mid).map(_.withNeighbor(maskedId, mask).values.toSet)

  /** Collect the neighbors of each device. */
  def collectNeighbors: Flow[Set[DeviceId]] = nbr(mid).toSet

  /** Collect the distances between each device and each of its neighbors. */
  def collectNeighborDistances: Flow[NeighborField[Double]] = nbrRange

  /** Collect the neighbors of each device, excluding the specified device from the collected neighbors. */
  def collectNeighborsExcept(neighborId: DeviceId): Flow[Set[DeviceId]] =
    nbr(mid).map(_.withoutNeighbor(neighborId).values.toSet)

  /** Collect the neighbors of each device, excluding the device itself from the collected neighbors. */
  def collectNeighborsExceptSelf: Flow[Set[DeviceId]] = nbr(mid).withoutSelf.toSet

  /** Collect the neighbors of each device, including the specified device to the collected neighbors. */
  def collectNeighborsIncluding(neighborId: DeviceId): Flow[Set[DeviceId]] =
    nbr(mid).map(_.withNeighbor(neighborId, neighborId).values.toSet)

  /** Collect the neighbors shared between each device and every other, sorted by id. */
  def collectSharedSortedNeighbors: Flow[Map[DeviceId, Seq[DeviceId]]] =
    nbr(nbr(mid).toSet).map(neighborSets =>
      val knownDevices: Set[DeviceId] = neighborSets.values.foldLeft(Set.empty[DeviceId])(_ ++ _)
      Map.from(knownDevices.map(deviceId => deviceId -> neighborSets.filter(_._2.contains(deviceId)).keys.toSeq.sorted))
    )

  /** Collect the neighbors of each device, sorted by id. */
  def collectSortedNeighbors: Flow[Seq[DeviceId]] = nbr(mid).map(_.values.toSeq.sorted)

  /**
   * Collect the neighbors of each device sorted by id and split the network using the specified boundary.
   *
   * @see [[splitNetworkAndCollectSortedNeighbors]], where the order of operation is reversed.
   */
  def collectSortedNeighborsAndSplitNetwork(boundary: DeviceId => Boolean): Flow[Seq[DeviceId]] =
    val thenValue = collectSortedNeighbors
    val elseValue = collectSortedNeighbors
    branch(mid.map(boundary)) { thenValue } { elseValue }

  /** Gradually compute all numbers in the specified range with the specified step, ends included. */
  def count(from: Int, to: Int, step: Int = 1): Flow[Int] =
    loop(from - step) { _.map(x => math.min(x + step, to)) }

  /** Count the number of neighbors of each device. */
  def countNeighbors: Flow[Int] = nbr(constant(1)).map(_.values.sum)

  /** Count the number of neighbors shared between each device and every other. */
  def countSharedNeighbors: Flow[Map[DeviceId, Int]] =
    collectSharedSortedNeighbors.map(_.map(_ -> _.size))

  /** Collect the id of all the devices in the network of each device. */
  def discovery: Flow[Set[DeviceId]] =
    loop(Set.empty[DeviceId]) { knownDevices =>
      liftTwice(nbr(knownDevices), nbr(mid))(_ + _)
        .map(_.values.foldLeft(Set.empty[DeviceId])(_ ++ _))
    }

  // From the paper: https://www.semanticscholar.org/reader/e56f679047e1c40482f8f04203c6abd275d77bf4
  /** Evaluate to true for all devices if the specified condition was true for any device at any time. */
  def ever(condition: Flow[Boolean]): Flow[Boolean] =
    share(constant(false)) { lift(_, condition)(_.values.foldLeft(_)(_ || _)) }

  /** Groups the devices in the system by proximity with the specified centroids. */
  def gradientPartition(centroids: Set[DeviceId])(using
    priorityPolicy: GradcastMessagePriorityPolicy[DeviceId] = GradcastMessagePriorityPolicy.lowest[DeviceId]
  ): Flow[Option[DeviceId]] =
    gradcast(mid.map(centroids), mid)

  /** As [[gradient]], but the specified obstacles prevent the propagation of the gradient. */
  def gradientWithObstacles(
    sources: Flow[Boolean],
    obstacles: Flow[Boolean],
    obstacleValue: Double = -1
  ): Flow[Double] =
    branch(obstacles) { constant(obstacleValue) } { gradient(sources) }

  /** Compute the maximum id of the network for each device. */
  def leaderElection: Flow[DeviceId] =
    loop(Int.MinValue) { leaderId => lift(mid, nbr(leaderId).max)(math.max) }

  /** Compute the maximum id of the neighbors for each device. */
  def maxNeighborId: Flow[Int] = nbr(mid).max

  /** As [[minChannel]], but sources and destinations are selected by their device id. */
  def minChannelFromIds(sources: Set[DeviceId], destinations: Set[DeviceId]): Flow[Boolean] =
    minChannel(mid.map(sources), mid.map(destinations))

  /** Compute the minimum and maximum id of the neighbors for each device. */
  def minMaxNeighborsId: Flow[(DeviceId, DeviceId)] =
    lift(minNeighborId, maxNeighborId)(_ -> _)

  /** Compute the minimum id of the neighbors for each device. */
  def minNeighborId: Flow[Int] = nbr(mid).min

  /** As [[minRedundantChannel]], but sources and destinations are selected by their device id. */
  def minRedundantChannelFromIds(
    sources: Set[DeviceId],
    destinations: Set[DeviceId],
    maxDeviation: Flow[Double]
  ): Flow[Boolean] =
    minRedundantChannel(mid.map(sources), mid.map(destinations), maxDeviation)

  /** As [[minRedundantChannel]], but sources and destinations are selected by their device id. */
  def multiplexConditionally[A](selector: DeviceId => Boolean, ifSelected: => A, otherwise: => A): Flow[A] =
    mux(mid.map(selector)) { constant(ifSelected) } { constant(otherwise) }

  /** Select one of the two specified values depending on the specified condition, using [[mux]]. */
  def multiplexAndCollectSortedNeighbors(boundary: DeviceId => Boolean): Flow[Seq[DeviceId]] =
    def collectSortedNeighbors: Flow[Seq[DeviceId]] = nbr(mid).map(_.values.toSeq.sorted)
    mux(mid.map(boundary)) { collectSortedNeighbors } { collectSortedNeighbors }

  /** Compute the sequences of values computed by the specified sequence of [[Flow Flow]]s. */
  def parallel[T](flows: Seq[Flow[T]]): Flow[Seq[T]] =
    flows.foldLeft(constant(Seq.empty))((acc, next) => lift(acc, next)(_ :+ _))

  /** Compute the sequences of values computed by the specified number of replicas of the specified [[Flow Flow]]. */
  def replicate[T](flow: => Flow[T], replicas: Int): Flow[Seq[T]] =
    parallel(Seq.range(0, replicas).map(_ => flow))

  /** Gradually compute the specified values for each device. */
  def sequence[T](xs: T*): Flow[T] =
    count(from = 0, to = xs.size - 1).map(xs.apply)

  /** As [[count]], but all devices count together sharing the maximum value produced so far. */
  def sharedCount(from: Int, to: Int, step: Int = 1): Flow[Int] =
    val start = from - step
    share(constant(Int.MinValue)) { maxCounts =>
      val newCount = share(constant(start)) { counts =>
        lift(counts, mid)(_.getOrElse(_, start) + step).map(math.min(_, to))
      }
      lift(maxCounts.max, newCount)(math.max)
    }

  /**
   * Apply the specified side-effect each time the specified [[Flow Flow]] produces a new export.
   *
   * @return the specified [[Flow Flow]].
   */
  def sideEffect[T](flow: Flow[T], onExport: PartialFunction[T, Unit]): Flow[T] =
    flow.map(t => { if onExport.isDefinedAt(t) then onExport(t); t })

  /** Compute the id of each device that is a source, or [[None]] otherwise. */
  def sourceMid: Flow[Option[DeviceId]] =
    mux(isSource){ mid.map(Some.apply) }{ constant(None) }

  /**
   * Collect the values computed by all the devices in the network by the specified [[Flow Flow]],
   * for each device.
   */
  def snapshot[T](flow: Flow[T]): Flow[Map[DeviceId, (Int, T)]] =
    loop(SnapshotData[T]()) { knownSnapshot =>
      val updatedSnapshot = lift(knownSnapshot, mid, flow)(_.updateEvent(_, _))
      nbr(updatedSnapshot).map(updatedSnapshots => SnapshotData.merge(updatedSnapshots.values))
    }.map(_.deviceEvents)

  /**
   * Split the network using the specified boundary and collect the neighbors of each device sorted by id.
   *
   * @see [[collectSortedNeighborsAndSplitNetwork]], where the order of operation is reversed.
   */
  def splitNetworkAndCollectSortedNeighbors(boundary: DeviceId => Boolean): Flow[Seq[DeviceId]] =
    branch(mid.map(boundary)) { collectSortedNeighbors } { collectSortedNeighbors }

  /**
   * As [[splitNetworkAndCollectSortedNeighbors]], but the network is split multiple times recursively
   * in non-overlapping subnetworks using the boundaries defined in the specified [[BinaryDecisionTree]].
   */
  def splitNetworkRecursivelyAndCollectSortedNeighbors(boundaryPolicy: BinaryDecisionTree[DeviceId]): Flow[Seq[DeviceId]] =
    def split(tree: Option[BinaryDecisionTree[DeviceId]]): Flow[Seq[DeviceId]] =
      tree.map(splitNetworkRecursivelyAndCollectSortedNeighbors).getOrElse(collectSortedNeighbors)
    branch(mid.map(boundaryPolicy.decision)) { split(boundaryPolicy.left) } { split(boundaryPolicy.right) }

  /**
   * As [[splitNetworkAndCollectSortedNeighbors]], but the network is split multiple times in overlapping
   * subnetworks using the specified boundaries.
   */
  def splitNetworkWithOverlapsAndCollectSortedNeighbors(boundaries: (DeviceId => Boolean)*): Flow[List[Seq[DeviceId]]] =
    boundaries
      .map(splitNetworkAndCollectSortedNeighbors)
      .foldLeft(constant(List.empty))((acc, next) => lift(acc, next)(_ :+ _))

  /** Compute the first id among the neighbors that is greater than the id of each device. */
  def successorNeighbor: Flow[Option[Int]] =
    lift(nbr(mid), mid)((neighbors, self) => neighbors.values.toSeq.sorted.find(_ > self))

  /** As [[successorNeighbor]], but only consider neighbors that are sources. */
  def successorSourceNeighbor: Flow[Option[Int]] =
    lift(nbr(mid), nbr(isSource), mid)((neighbors, sources, self) =>
      neighbors.values.toSeq.sorted.find(nbr => nbr > self && sources.getOrElse(nbr, false))
    )

  /** As [[successorSourceNeighborNested]], but use nested [[lift]]s. */
  def successorSourceNeighborNested: Flow[Option[Int]] =
    lift(
      lift(nbr(mid), nbr(isSource))((neighbors, sources) => (self: DeviceId) =>
        neighbors.values.toSeq.sorted.find(nbr => nbr > self && sources.getOrElse(nbr, false))
      ),
      mid
    )(_.apply(_))

  /** Compute the values of the specified [[Flow Flow]] bound to the round they have been computed. */
  def zipWithRound[T](flow: Flow[T], startingRound: Int = 0): Flow[(Int, T)] =
    loop((startingRound - 1, Option.empty[T])) {
      lift(_, flow) {
        case ((round, prev), next) if !prev.contains(next) => (round + 1, Some(next))
        case (prevResult, _) => prevResult
      }
    }.map(_ -> _.get)

/** Companion object of [[FraspSamples]]. */
object FraspSamples:
  /** The data structure representing a snapshot. */
  private case class SnapshotData[E](deviceEvents: Map[Int, (Int, E)] = Map.empty):
    def updateEvent(device: Int, event: E): SnapshotData[E] =
      if deviceEvents.get(device).map(_._2).contains(event) then this else
        val newEntry: (Int, E) = deviceEvents.get(device).map((t, _) => (t + 1) -> event).getOrElse(0 -> event)
        SnapshotData(deviceEvents + (device -> newEntry))
  /** Companion object of [[SnapshotData]]. */
  private object SnapshotData:
    def merge[E](snapshots: Iterable[SnapshotData[E]]): SnapshotData[E] =
      snapshots.foldLeft(SnapshotData())((s1, s2) => SnapshotData(
        s1.deviceEvents ++
          s2.deviceEvents.filter((device, te2) => !s1.deviceEvents.get(device).exists(te1 => te1._1 >= te2._1))
      ))

  /**
   * A binary tree used for classifying data through a sequence of classification rules.
   *
   * @param decision the classification rule of this tree.
   * @param left     the [[BinaryDecisionTree BinaryDecisionTree]] applied to the data if the
   *                 classification rule of this [[BinaryDecisionTree BinaryDecisionTree]] holds.
   * @param right    the [[BinaryDecisionTree BinaryDecisionTree]] applied to the data if the
   *                 classification rule of this [[BinaryDecisionTree BinaryDecisionTree]] does
   *                 not hold.
   * @tparam T the type of data classified by this [[BinaryDecisionTree BinaryDecisionTree]].
   */
  case class BinaryDecisionTree[T](
    decision: T => Boolean,
    left: Option[BinaryDecisionTree[T]] = None,
    right: Option[BinaryDecisionTree[T]] = None
  )
