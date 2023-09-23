package it.unibo.distributedfrp.simulation.simulator

import it.unibo.distributedfrp.simulation.{Environment, SimulationIncarnation}
import nz.sodium

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

/**
 * A basic implementation of a [[Simulator]].
 *
 * @param incarnation the [[Incarnation]] for which this [[BasicSimulator]] is capable of
 *                    configuring [[Simulator.Simulation Simulation]]s.
 * @param executor the [[ExecutionContext]] used to execute the [[Simulator.Simulation Simulation]]s
 *                 configured by this [[BasicSimulator]].
 */
class BasicSimulator(
  override val incarnation: SimulationIncarnation,
  executor: ExecutionContext
) extends Simulator[SimulationIncarnation]:
  override def simulation[A](flow: Flow[A]): Simulation[A] =
    Simulation(executionStream =>
      val deviceMap: Map[DeviceId, Context] = createDeviceMap()
      deviceMap
        .map((deviceId, device) => flow.run(Seq.empty)(using device).map(deviceExport => deviceId -> deviceExport))
        .foreach(deviceExportStream =>
          deviceExportStream.listen((deviceId, deviceExport) =>
            executor.execute(() =>
              // Notify the user of the computational step
              executionStream.send(deviceId -> deviceExport)
              // Notify the neighbors of the computational step
              incarnation.environment.neighbors(deviceId).foreach(neighborId =>
                deviceMap(neighborId).receiveExport(deviceId, deviceExport)
              )
            )
          )
        )
    )

  /**
   * @return a new [[Map]] from [[DeviceId DeviceId]]s to [[Context Device]]s,
   *         generated from the [[Environment]] of this [[Simulator]].
   */
  private def createDeviceMap(): Map[DeviceId, Context] =
    Seq.range(0, incarnation.environment.nDevices)
      .map(deviceId => deviceId -> incarnation.context(deviceId))
      .toMap

/** Companion object of [[BasicSimulator]]. */
object BasicSimulator:
  /**
   * Create a new [[BasicSimulator]] for the specified [[Incarnation]].
   *
   * @param incarnation the [[Incarnation]] for which the new [[BasicSimulator]] is capable of
   *                    configuring [[Simulator.Simulation Simulation]]s.
   * @param executor    the [[ExecutionContext]] used to execute the [[Simulator.Simulation Simulation]]s
   *                    configured by the new [[BasicSimulator]].
   * @return a new [[BasicSimulator]] for the specified [[Incarnation]].
   */
  def apply(incarnation: SimulationIncarnation)(using executor: ExecutionContext): Simulator[SimulationIncarnation] =
    new BasicSimulator(incarnation, executor)

  /**
   * Create a new deterministic [[BasicSimulator]] for the specified [[Incarnation]].
   *
   * @param incarnation the [[Incarnation]] for which the new [[BasicSimulator]] is capable of
   *                    configuring [[Simulator.Simulation Simulation]]s.
   * @return a new deterministic [[BasicSimulator]] for the specified [[Incarnation]].
   * @note a deterministic [[BasicSimulator]] allows to run the same experiment multiple times
   *       under the same conditions obtaining the same result. Particularly useful for testing.
   */
  def deterministic(incarnation: SimulationIncarnation): Simulator[SimulationIncarnation] =
    BasicSimulator(incarnation)(using ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor()))

  /**
   * Create a new parallel [[BasicSimulator]] for the specified [[Incarnation]].
   *
   * @param incarnation the [[Incarnation]] for which the new [[BasicSimulator]] is capable of
   *                    configuring [[Simulator.Simulation Simulation]]s.
   * @return a new parallel [[BasicSimulator]] for the specified [[Incarnation]].
   * @note a parallel [[BasicSimulator]] is not necessarily deterministic. See [[deterministic]]
   *       for more information.
   */
  def parallel(incarnation: SimulationIncarnation): Simulator[SimulationIncarnation] =
    BasicSimulator(incarnation)(using ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))