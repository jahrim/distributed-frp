package it.unibo.distributedfrp.simulation.simulator.concurrent

import it.unibo.distributedfrp.frp.StreamExtension.Stream
import it.unibo.distributedfrp.simulation.simulator.Simulator
import it.unibo.distributedfrp.simulation.simulator.Simulator.WithIncarnation
import it.unibo.distributedfrp.simulation.simulator.step.StepSimulator
import it.unibo.distributedfrp.utils.Observables.Subscription

import scala.concurrent.ExecutionContext
import scala.util.Try

/** A [[Simulator]] configuring simulations that supports multi-threaded execution. */
trait ConcurrentSimulator extends Simulator with ConcurrentSimulator.Components:
  import incarnation.{*, given}

  override type Configuration[A] <: ConcurrentSimulationConfiguration[A]

  private object Underlying extends StepSimulator with WithIncarnation[incarnation.type](incarnation):
    override type Configuration[A] = StepSimulationConfiguration[A]

  override def simulation[A](flow: Flow[A])(using configuration: Configuration[A]): Simulation[A] =
    val underlyingConfiguration = configuration
    new Simulation[A]:
      import underlyingConfiguration.*
      given ExecutionContext = executor

      private val _underlying: ConcurrentSimulator.this.Underlying.AsyncStepSimulation[A] =
        ConcurrentSimulator.this.Underlying.asyncSimulation(flow)(using
          ConcurrentSimulator.this.Underlying.StepSimulationConfiguration(
            environment = environment,
            haltPolicy = _ => underlyingConfiguration.haltPolicy.apply(this),
            logger = logger
          )
        )
      private var _subscriptions: Seq[Subscription] = Seq()

      override def configuration: Configuration[A] = underlyingConfiguration
      override def exported: Stream[CollectiveExportMap[A]] = this._underlying.exported
      override protected def startBehavior(): Unit =
        this._underlying.start()
        this._subscriptions = this._subscriptions :+ this._underlying.ready.subscribe(_ => this._underlying.asyncNext())
        Seq.range(0, environment.nDevices).foreach(_ => this._underlying.asyncNext())
      override protected def stopBehavior(): Unit =
        this._subscriptions.foreach(_.unsubscribe())
        Try(this._underlying.stop())

/** Companion object of [[ConcurrentSimulator]]. */
object ConcurrentSimulator:
  /** A mixin for providing the components of a [[ConcurrentSimulator]] to a [[Simulator Simulator]]. */
  trait Components
    extends HaltPolicyComponent
       with ConcurrentSimulationConfigurationComponent:
    self: Simulator =>
