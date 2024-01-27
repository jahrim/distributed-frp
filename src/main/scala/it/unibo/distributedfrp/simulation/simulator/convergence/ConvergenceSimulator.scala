package it.unibo.distributedfrp.simulation.simulator.convergence

import it.unibo.distributedfrp.frp.StreamExtension.Stream
import it.unibo.distributedfrp.simulation.simulator.*

import scala.concurrent.{ExecutionContext, Future}

/** A [[Simulator]] that is capable of evaluating the limit of a flow. */
trait ConvergenceSimulator:
  self: Simulator =>
  import incarnation.{*, given}

  /**
   * @param flow          the specified [[incarnation.Flow Flow]].
   * @param configuration the specified [[Configuration Configuration]].
   * @return a [[Future]] containing the latest values computed by all
   *         the devices running the specified [[Flow Flow]] using the
   *         specified [[Configuration Configuration]].
   *         The [[Future]] completes when the simulation of the specified
   *         [[Flow Flow]] is [[Simulation.termination terminated]].
   * @note since the simulation may be non-deterministic it is advised to
   *       repeat the evaluation a sufficient number of times in order to
   *       provide stochastic significance to the results.
   */
  def computeLimit[A](flow: Flow[A])(using configuration: Configuration[A]): Future[CollectiveResultMap[A]]

  /** Alias for [[computeLimit]]. */
  def lim[A](flow: Flow[A])(using configuration: Configuration[A]): Future[CollectiveResultMap[A]] =
    this.computeLimit(flow)

  /**
   * @param flows                 the specified [[incarnation.Flow Flow]]s.
   * @param configurationSupplier the specified [[ConfigurationSupplier ConfigurationSupplier]].
   * @return a [[Future]] containing the latest values computed by all
   *         the devices running the specified [[Flow Flow]]s using the
   *         specified [[ConfigurationSupplier ConfigurationSupplier]].
   *         The [[Future]] completes when the simulations of the specified
   *         [[Flow Flow]]s are all [[Simulation.termination terminated]].
   * @note since the simulation may be non-deterministic it is advised to
   *       repeat the evaluation a sufficient number of times in order to
   *       provide stochastic significance to the results.
   */
  def computeLimits[A](flows: (Configuration[A] ?=> Flow[A])*)(using
    configurationSupplier: () => Configuration[A]
  ): Future[Seq[CollectiveResultMap[A]]] =
    given ExecutionContext = ExecutionContext.parasitic
    flows.map { flow =>
      given Configuration[A] = configurationSupplier()
      this.computeLimit(flow)
    }.foldLeft(Future.successful(Seq.empty[CollectiveResultMap[A]]))((accFuture, nextFuture) =>
      accFuture.flatMap(acc => nextFuture.map(next => acc :+ next))
    )

/** Companion object of [[ConvergenceSimulator]]. */
object ConvergenceSimulator:
  /**
   * The [[step.StepSimulator StepSimulator]] implementation of a
   * [[ConvergenceSimulator ConvergenceSimulator]].
   */
  trait StepSimulator extends ConvergenceSimulator with step.StepSimulator:
    import incarnation.{*, given}
    override def computeLimit[A](flow: Flow[A])(using configuration: Configuration[A]): Future[CollectiveResultMap[A]] =
      given ExecutionContext = ExecutionContext.parasitic
      val simulation = this.simulation(flow)
      val computation = simulation.computedByAll
      val computationMonitor = Stream.monitor(computation, memory = 1)
      simulation.start()
      while (simulation.isRunning) { simulation.next() }
      simulation.termination.map(_ => computationMonitor.eventLog.lastOption.getOrElse(Map()))
