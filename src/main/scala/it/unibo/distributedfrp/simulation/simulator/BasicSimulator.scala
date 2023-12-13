package it.unibo.distributedfrp.simulation.simulator

import it.unibo.distributedfrp.simulation.SimulationIncarnation
import nz.sodium

import java.util.concurrent.{ExecutorService, Executors}

/**
 * A basic implementation of a [[Simulator]].
 *
 * @param incarnation the [[SimulationIncarnation]] for which this [[BasicSimulator]]
 *                    is capable of configuring [[Simulator.Simulation Simulation]]s.
 */
class BasicSimulator(override val incarnation: SimulationIncarnation)
  extends Simulator[SimulationIncarnation]:
  override def simulation[A](flow: Flow[A]): Simulation[A] = new Simulation[A]:
    private var _listeners: Iterable[sodium.Listener] = Seq()
    private val _executor: ExecutorService = Executors.newSingleThreadExecutor()
    private val _deviceMap: Map[DeviceId, Context] =
      Seq.range(0, incarnation.environment.nDevices)
        .map(deviceId => deviceId -> incarnation.context(deviceId))
        .toMap
    override protected def startBehavior(notifyComputationStep: IndividualExport[A] => Unit): Unit =
      this._listeners =
        this._deviceMap.map((deviceId, device) =>
          flow.run(Seq.empty)(using device).listen(deviceExport =>
            if isRunning then
              this._executor.execute(() =>
                // Notify the user of the computational step
                notifyComputationStep(deviceId -> deviceExport)
                // Notify the neighbors of the computational step
                incarnation.environment.neighbors(deviceId).foreach(neighborId =>
                  this._deviceMap(neighborId).receiveExport(deviceId, deviceExport)
                )
              )
          )
        )

    override protected def stopBehavior(): Unit =
      this._executor.synchronized { this._executor.shutdown() }
      this._listeners.foreach(_.unlisten())

    /** @return true if this [[Simulation Simulation]] is running, false otherwise. */
    private def isRunning: Boolean =
      !this._executor.synchronized { this._executor.isShutdown }
