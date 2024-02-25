package it.unibo.distributedfrp.simulation.simulator.step

import it.unibo.distributedfrp.frp.StreamExtension.Stream
import it.unibo.distributedfrp.frp.StreamSinkExtension.StreamSink
import it.unibo.distributedfrp.simulation.simulator.Simulator
import it.unibo.distributedfrp.utils.Observables.Observable
import it.unibo.distributedfrp.frp.IncrementalCellSink

import scala.concurrent.{ExecutionContext, Future}

/** A [[Simulator]] configuring simulations that can be executed step-by-step by the user. */
trait StepSimulator extends Simulator with StepSimulator.Components:
  import incarnation.{*, given}

  override type Configuration[A] <: StepSimulationConfiguration[A]

  override def simulation[A](flow: Flow[A])(using configuration: Configuration[A]): StepSimulation[A] =
    asyncSimulation(flow)

  /** As [[simulation]], but returns a thread-safe [[StepSimulation StepSimulation]]. */
  def asyncSimulation[A](flow: Flow[A])(using configuration: Configuration[A]): AsyncStepSimulation[A] =
    val underlyingConfiguration = configuration
    new AsyncStepSimulation[A]:
      import underlyingConfiguration.*

      private val _deviceNeighborsMap: Map[DeviceId, IncrementalCellSink[Map[DeviceId, NeighborState]]] =
        Seq.range(0, environment.nDevices).map(_ -> IncrementalCellSink(initValue = Map(), calm = true)).toMap
      private val _deviceMap: Map[DeviceId, Context] =
        this._deviceNeighborsMap.map((deviceId, neighbors) =>
          deviceId -> SimulationContext(deviceId, neighbors.cell, environment)
        )
      private val _steps: StreamSink[CollectiveExportMap[A]] = StreamSink()
      private val _exportScheduler: ExportScheduler[A] = ExportScheduler(this._deviceMap)
      private var _listeners: Iterable[nz.sodium.Listener] = Seq()

      override def configuration: Configuration[A] = underlyingConfiguration
      override def exportedSteps: Stream[CollectiveExportMap[A]] = this._steps
      override protected def startBehavior(): Unit =
        logger.logActivity("Setup simulation") {
          this._listeners =
            this._deviceMap.map((deviceId, device) =>
              flow.run(Seq.empty)(using device).listen(deviceExport =>
                logger.logActivity(s"Schedule: ${deviceId -> deviceExport.root}") {
                  this._exportScheduler.schedule(deviceId -> deviceExport)
                }
              )
            )
          haltPolicy.apply(this)
        }
      override protected def stopBehavior(): Unit =
        logger.logActivity("Stop simulation") {
          this._listeners.foreach(_.unlisten())
        }
      override def asyncNext()(using executor: ExecutionContext): Future[Unit] =
        logger.logActivity("Continue simulation") {
          if this.isRunning then
            this._exportScheduler.next(currentExport =>
              currentExport.foreach((sender, message) => exportToNeighbors(sender, message))
              exportToUser(currentExport)
            )
          else
            Future.failed(IllegalStateException("Cannot continue a simulation that is not running."))
        }
      override def ready: Observable[Unit] = this._exportScheduler.ready

      /**
       * Send the specified message from the specified sender to its neighbors.
       *
       * @param sender  the specified sender.
       * @param message the specified message.
       */
      private def exportToNeighbors(sender: DeviceId, message: Export[A])(using ExecutionContext): Unit =
        logger.logActivity(s"Export to neighbors: ${sender -> message.root}") {
          environment.neighbors(sender).foreach(neighbor =>
            Future {
              this._deviceNeighborsMap(neighbor).update(neighborNeighborsMap =>
                neighborNeighborsMap + (
                  sender -> SimulationNeighborState(
                    selfId = neighbor,
                    neighborId = sender,
                    exported = message,
                    environment = environment
                  )
                )
              )
            }
          )
        }

      /**
       * Send the specified execution step to the user.
       *
       * @param step the specified execution step.
       */
      private def exportToUser(step: Option[(DeviceId, Export[A])]): Unit =
        logger.logActivity(s"Export to user: ${step.map(_ -> _.root)}") {
          this._steps.send(step.map((sender, message) => Map(sender -> message)).getOrElse(Map()))
        }

/** Companion object of [[StepSimulator]]. */
object StepSimulator:
  /** A mixin for providing the components of a [[StepSimulator]] to a [[Simulator Simulator]]. */
  trait Components
    extends StepSimulationComponent
       with ExportSchedulerComponent
       with AsyncStepSimulationComponent
       with HaltPolicyComponent
       with StepSimulationConfigurationComponent:
    self: Simulator =>