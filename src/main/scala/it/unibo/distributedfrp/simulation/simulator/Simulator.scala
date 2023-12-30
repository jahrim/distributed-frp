package it.unibo.distributedfrp.simulation.simulator

import it.unibo.distributedfrp.core.Incarnation
import it.unibo.distributedfrp.frp.FrpExtensions.{*, given}
import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.frp.StreamSinkExtension.StreamSink
import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation
import nz.sodium

/**
 * A simulator capable of configuring simulations for a specific [[SimulationIncarnation]].
 */
trait Simulator[I <: SimulationIncarnation]:
  /**
   * The [[SimulationIncarnation]] for which this [[Simulator]] is capable of
   * configuring [[Simulation Simulation]]s.
   */
  val incarnation: I
  import incarnation.{*, given}

  /**
   * The [[Export Export]] of a specific [[incarnation.Context Device]]
   * at a given time.
   */
  type IndividualExport[+A] = (DeviceId, Export[A])
  /**
   * The result computed by a specific [[incarnation.Context Device]]
   * at a given time.
   */
  type IndividualResult[+A] = (DeviceId, A)
  /**
   * The [[Export Export]]s of all the [[incarnation.Context Device]]s at a
   * given time.
   */
  type CollectiveExportMap[+A] = Map[DeviceId, Export[A]]
  /**
   * The results computed by all the [[incarnation.Context Device]]s
   * at a given time.
   */
  type CollectiveResultMap[+A] = Map[DeviceId, A]

  /**
   * Configure a new [[Simulation Simulation]] for executing the specified [[Flow]]
   * in the specified [[Environment]].
   *
   * @param flow        the specified [[incarnation.Flow Flow]].
   * @param environment the specified [[Environment]].
   * @tparam A the type of results produced by the specified [[incarnation.Flow Flow]].
   * @return a new [[Simulation Simulation]] for executing the specified [[Flow]].
   */
  def simulation[A](flow: Flow[A])(using environment: Environment): Simulation[A]

  /**
   * A simulation that executes a [[Flow]], letting the user observe its execution.
   *
   * @tparam A the type of results produced by the [[incarnation.Flow Flow]].
   */
  @FunctionalInterface
  trait Simulation[A]:
    private val _execution: StreamSink[IndividualExport[A]] = sodium.StreamSink[IndividualExport[A]]()
    private var _started: Boolean = false
    private var _stopped: Boolean = false

    /**
     * Start this [[Simulation Simulation]].
     *
     * When this method is called, the [[startBehavior]] method of the concrete implementation
     * of this [[Simulation Simulation]] will be called and the [[Stream Stream]]s exposed
     * by this [[Simulation Simulation]] will start generating their corresponding events.
     *
     * The user should attach his listeners to the [[Stream Stream]]s exposed by this
     * [[Simulation Simulation]] before starting the simulation in order to prevent the loss of
     * events.
     *
     * @note this method should not and cannot be overridden. To change the starting behavior of a
     *       [[Simulation Simulation]], override the [[startBehavior]] method instead.
     */
    final def start(): Unit =
      if this._started then
        throw IllegalStateException("Cannot start a simulation that has already started.")
      else
        this._started = true
        sodium.Transaction.run(() =>
          this.startBehavior((deviceId, deviceExport) =>
            if !synchronized(this._stopped)
            then this._execution.send(deviceId -> deviceExport)
          )
        )

    /**
     * Stop this [[Simulation Simulation]].
     *
     * When this method is called, the [[stopBehavior]] method of the concrete implementation
     * of this [[Simulation Simulation]] will be called and the [[Stream Stream]]s exposed
     * by this [[Simulation Simulation]] will stop generating their corresponding events.
     *
     * @note this method should not and cannot be overridden. To change the stopping behavior of a
     *       [[Simulation Simulation]], override the [[stopBehavior]] method instead.
     */
    final def stop(): Unit =
      if !this._started then
        throw IllegalStateException("Cannot stop a simulation that hasn't started yet.")
      else if this._stopped then
        throw IllegalStateException("Cannot stop a simulation that has already stopped.")
      else
        this._stopped = true
        this.stopBehavior()

    /**
     * The starting behavior of this [[Simulation Simulation]], configuring its execution.
     *
     * @param notifyComputationStep a function that must be called to notify the users of
     *                              each next computation step of this [[Simulation Simulation]].
     */
    protected def startBehavior(notifyComputationStep: IndividualExport[A] => Unit): Unit

    /** The stopping behavior of this [[Simulation Simulation]], configuring its termination. */
    protected def stopBehavior(): Unit

    /**
     * @return the [[Stream Stream]] of the [[IndividualExport IndividualExport]]s generated
     *         by the [[incarnation.Context Device]]s as this [[Simulation Simulation]] is
     *         executed.
     */
    def exported: Stream[IndividualExport[A]] =
      this._execution
    /**
     * @return the [[Stream Stream]] of the [[IndividualResult IndividualResult]]s generated
     *         by the [[incarnation.Context Device]]s as this [[Simulation Simulation]] is
     *         executed.
     */
    def computed: Stream[IndividualResult[A]] =
      this.exported.map(_ -> _.root)

    /**
     * @param deviceId the specified id.
     * @return the [[Stream Stream]] of the [[Export Export]]s generated by the
     *         [[Context Device]] with the specified id.
     */
    def exportedBy(deviceId: DeviceId): Stream[Export[A]] =
      this.exported.filter(_._1 == deviceId).map(_._2)

    /**
     * @param deviceId the specified id.
     * @return the [[Stream Stream]] of the [[Result Result]]s generated by the
     *         [[Context Device]] with the specified id.
     */
    def computedBy(deviceId: DeviceId): Stream[A] =
      this.exportedBy(deviceId).map(_.root)

    /**
     * @return the [[Stream Stream]] of the [[CollectiveExportMap CollectiveExportMap]]
     *         generated by collecting the [[Export Export]]s of all the
     *         [[incarnation.Context Device]]s as this [[Simulation Simulation]] is executed.
     */
    def exportedByAll: Stream[CollectiveExportMap[A]] =
      this.exported.fold(Map())(_ + _)

    /**
     * @return the [[Stream Stream]] of the [[CollectiveResultMap CollectiveResultMap]]
     *         generated by collecting the [[Result Result]]s of all the
     *         [[incarnation.Context Device]]s as this [[Simulation Simulation]] is executed.
     */
    def computedByAll: Stream[CollectiveResultMap[A]] =
      this.exportedByAll.map(_.map(_ -> _.root))
