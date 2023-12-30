package it.unibo.distributedfrp.core.convergence.algorithms

import it.unibo.distributedfrp.core.convergence.ConvergenceTest
import it.unibo.distributedfrp.simulation.environment.Environment.euclideanGrid
import it.unibo.distributedfrp.simulation.environment.{EnvironmentFactory, EnvironmentWithTags}

/** A [[ConvergenceTest]] for the network snapshot algorithm. */
class SnapshotTest extends ConvergenceTest.WithDefaults:
  private val Snapshot = symbol("snapshot")

  import defaultSimulator.incarnation.{*, given}
  private given EnvironmentFactory[EnvironmentWithTags] = () => EnvironmentWithTags(euclideanGrid(5, 5))

  private case class SnapshotData[E](deviceEvents: Map[DeviceId, (Int, E)] = Map.empty):
    def updateEvent(device: DeviceId, event: E): SnapshotData[E] =
      if deviceEvents.get(device).map(_._2).contains(event) then this else
        val newEntry: (Int, E) = deviceEvents.get(device).map((t, _) => (t + 1) -> event).getOrElse(0 -> event)
        SnapshotData(deviceEvents + (device -> newEntry))
  private object SnapshotData:
    def merge[E](snapshots: Iterable[SnapshotData[E]]): SnapshotData[E] =
      snapshots.foldLeft(SnapshotData())((s1, s2) => SnapshotData(
        s1.deviceEvents ++
        s2.deviceEvents.filter((device, te2) => !s1.deviceEvents.get(device).exists(te1 => te1._1 >= te2._1))
      ))
  private def snapshot[T](flow: Flow[T]): Flow[SnapshotData[T]] =
    loop(SnapshotData()) { knownSnapshot =>
      val updatedSnapshot = lift(knownSnapshot, mid, flow)(_.updateEvent(_, _))
      nbr(updatedSnapshot).map(updatedSnapshots => SnapshotData.merge(updatedSnapshots.values))
    }
  private def count(from: Int, to: Int, step: Int = 1): Flow[Int] =
    loop(from - step) { _.map(x => math.min(x + step, to)) }

  Snapshot should "collect the computation of all the devices in a network" in convergenceTest(
    simulator = defaultSimulator,
    flow = snapshot(count(from = 0, to = 10)),
    limit = {
      val (lastTime, lastEvent) = (10, 10)
      val snapshot: SnapshotData[Int] = SnapshotData(Set.range(0, 25).map(_ -> (lastTime, lastEvent)).toMap)
      Seq.range(0, 25).map(_ -> snapshot).toMap
    }
  )
