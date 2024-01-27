package it.unibo.distributedfrp.simulation.simulator

import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.utils.Cache

import scala.concurrent.{Future, Promise}

/** A mixin for providing the concept of simulation to a [[Simulator Simulator]]. */
trait Simulation:
  self: Simulator with SimulationConfiguration =>
  import incarnation.{*, given}

  /** The [[Export Export]]s of a group of [[Context Device]]s. */
  type CollectiveExportMap[+A] = Map[DeviceId, Export[A]]
  /** The results computed by a group of [[Context Device]]s. */
  type CollectiveResultMap[+A] = Map[DeviceId, A]

  /**
   * A simulation that executes a [[Flow Flow]], letting the user observe its execution.
   *
   * @tparam A the type of results produced by the [[Flow Flow]].
   */
  trait Simulation[A]:
    private val _termination: Promise[Unit] = Promise()
    private var _started: Boolean = false
    private var _stopped: Boolean = false

    /** @return the [[Configuration Configuration]] of this simulation. */
    def configuration: Configuration[A]
    /**
     * @return the [[Stream Stream]] of the individual exports generated by the [[Context Device]]s
     *         as this [[Simulation Simulation]] is executed.
     * @note the events of the [[Stream Stream]] are [[CollectiveExportMap]]s that contains all the
     *       simultaneous individual exports generated by different devices at a given instance in
     *       the simulation.
     */
    def exported: Stream[CollectiveExportMap[A]]

    /** The starting behavior of this [[Simulation Simulation]], configuring its execution. */
    protected def startBehavior(): Unit = {}
    /** The stopping behavior of this [[Simulation Simulation]], configuring its termination. */
    protected def stopBehavior(): Unit = {}

    /**
     * @return the [[Stream Stream]] of the individual results generated by the [[Context Device]]s
     *         as this [[Simulation Simulation]] is executed.
     * @note the events of the [[Stream Stream]] are [[CollectiveExportMap]]s that contains all the
     *       simultaneous individual results generated by different devices at a given instance in
     *       the [[Simulation Simulation]].
     */
    final def computed: Stream[CollectiveResultMap[A]] = this._cachedComputed
    private lazy val _cachedComputed: Stream[CollectiveResultMap[A]] = this.exported.map(_.map(_ -> _.root))

    /**
     * @param deviceId the specified id.
     * @return the [[Stream Stream]] of the [[Export Export]]s generated by the
     *         [[Context Device]] with the specified id.
     */
    final def exportedBy(deviceId: DeviceId): Stream[Export[A]] = this._cachedExportedBy(deviceId)
    private lazy val _cachedExportedBy: DeviceId => Stream[Export[A]] =
      Cache.cached(deviceId => this.exported.filter(_.contains(deviceId)).map(_(deviceId)))
    /**
     * @param deviceId the specified id.
     * @return the [[Stream Stream]] of the results generated by the
     *         [[Context Device]] with the specified id.
     */
    final def computedBy(deviceId: DeviceId): Stream[A] = this._cachedComputedBy(deviceId)
    private val _cachedComputedBy: DeviceId => Stream[A] = Cache.cached(this.exportedBy(_).map(_.root))
    /**
     * @return the [[Stream Stream]] of the [[CollectiveExportMap CollectiveExportMap]]
     *         generated by collecting the [[Export Export]]s of all the [[Context Device]]s
     *         as this [[Simulation Simulation]] is executed.
     * @note the events of the [[Stream Stream]] are [[CollectiveExportMap]]s that combine
     *       all the exports generated so far by all the devices in the [[Simulation Simulation]].
     */
    final def exportedByAll: Stream[CollectiveExportMap[A]] = this._cachedExportedByAll
    private lazy val _cachedExportedByAll: Stream[CollectiveExportMap[A]] = this.exported.fold(Map())(_ ++ _)
    /**
     * @return the [[Stream Stream]] of the [[CollectiveResultMap CollectiveResultMap]]
     *         generated by collecting the results of all the [[Context Device]]s as this
     *         [[Simulation Simulation]] is executed.
     * @note the events of the [[Stream Stream]] are [[CollectiveExportMap]]s that combine
     *       all the results generated so far by all the devices in the [[Simulation Simulation]].
     */
    final def computedByAll: Stream[CollectiveResultMap[A]] = this._cachedComputedByAll
    private lazy val _cachedComputedByAll: Stream[CollectiveResultMap[A]] = this.exportedByAll.map(_.map(_ -> _.root))

    /** @return true if this [[Simulation Simulation]] is running, false otherwise. */
    final def isRunning: Boolean = synchronized { this._started && !this._stopped }
    /** @return a [[Future]] that completes when this [[Simulation Simulation]] has terminated. */
    final def termination: Future[Unit] = this._termination.future
    /**
     * Start this [[Simulation Simulation]].
     *
     * When this method is called, the [[startBehavior]] method of the concrete implementation
     * of this [[Simulation Simulation]] will be called and the [[Stream Stream]]s exposed
     * by this [[Simulation Simulation]] will start generating their corresponding events.
     *
     * The user should attach his listeners to the [[Stream Stream]]s exposed by this
     * [[Simulation Simulation]] before starting the simulation in order to prevent the loss
     * of events.
     *
     * @note this method should not and cannot be overridden. To change the starting behavior
     *       of a [[Simulation Simulation]], override the [[startBehavior]] method instead.
     */
    final def start(): Unit = synchronized {
      if this._started then
        throw IllegalStateException("Cannot start a simulation that has already started.")
      else
        this._started = true
        this.startBehavior()
    }
    /**
     * Stop this [[Simulation Simulation]].
     *
     * When this method is called, the [[stopBehavior]] method of the concrete implementation
     * of this [[Simulation Simulation]] will be called and the [[Stream Stream]]s exposed
     * by this [[Simulation Simulation]] will stop generating their corresponding events.
     *
     * @note this method should not and cannot be overridden. To change the stopping behavior
     *       of a [[Simulation Simulation]], override the [[stopBehavior]] method instead.
     */
    final def stop(): Unit = synchronized {
      if !this.isRunning then
        throw IllegalStateException("Cannot stop a simulation that is not running.")
      else
        this._stopped = true
        this.stopBehavior()
        this._termination.success(())
    }
