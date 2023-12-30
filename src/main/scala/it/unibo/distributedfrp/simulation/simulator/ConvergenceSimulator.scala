package it.unibo.distributedfrp.simulation.simulator

import it.unibo.distributedfrp.frp.FiniteStreamExtension.*
import it.unibo.distributedfrp.frp.FiniteStreamExtension.FiniteEvent.*
import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.simulation.environment.EnvironmentFactory
import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation

import scala.concurrent.{ExecutionContext, Future}

/**
 * A [[Simulator]] that is capable of evaluating the limit of a flow.
 * @tparam I the type of [[Incarnation]] used by this [[Simulator]].
 */
trait ConvergenceSimulator[I <: SimulationIncarnation] extends Simulator[I]:
  import incarnation.{*, given}

  /**
   * @param flow the specified [[Flow Flow]].
   * @param environment the given [[Environment]].
   * @return a [[Future]] containing the latest values computed by all
   *         the devices running the specified [[Flow Flow]] in the given
   *         [[Environment]].
   *         The [[Future]] completes when the simulation of the specified
   *         [[Flow Flow]] is completed, using the given [[HaltPolicy HaltPolicy]].
   * @note since the simulation may be non-deterministic it is advised to repeat
   *       the evaluation a sufficient number of times in order to provide
   *       stochastic significance to the results.
   */
  def computeLimit[A](flow: Flow[A])(using
    environment: Environment,
    haltPolicy: HaltPolicy[CollectiveResultMap[A]],
    executor: ExecutionContext
  ): Future[CollectiveResultMap[A]]

  /** Alias for [[computeLimit]]. */
  def lim[A](flow: Flow[A])(using
    environment: Environment,
    haltPolicy: HaltPolicy[CollectiveResultMap[A]],
    executor: ExecutionContext
  ): Future[CollectiveResultMap[A]] =
    this.computeLimit(flow)

  /**
   * @param flows the specified [[Flow Flow]]s.
   * @param environmentFactory the specified [[EnvironmentFactory]].
   * @return a [[Future]] containing a sequence of the limits of the specified
   *         [[Flow]]s, computed using [[computeLimit]].
   *         The [[Future]] completes when the simulations of the specified
   *         [[Flow Flow]]s are completed, using the given [[HaltPolicy HaltPolicy]].
   *         The specified [[EnvironmentFactory]] is used to generate a clean
   *         [[Environment]] for each simulation.
   * @note since the simulation may be non-deterministic it is advised to repeat
   *       the evaluation a sufficient number of times in order to provide
   *       stochastic significance to the results.
   */
  def computeLimits[A](flows: (Environment ?=> Flow[A])*)(using
    environmentFactory: EnvironmentFactory[Environment],
    haltPolicy: HaltPolicy[CollectiveResultMap[A]],
    executor: ExecutionContext
  ): Future[Seq[CollectiveResultMap[A]]] =
    flows
      .map { flow => given Environment = environmentFactory.newInstance(); this.computeLimit(flow) }
      .foldLeft(Future.successful(Seq.empty[CollectiveResultMap[A]]))((accFuture, nextFuture) =>
        accFuture.flatMap(acc => nextFuture.map(next => acc :+ next))
      )

/** Companion object of [[ConvergenceSimulator]]. */
object ConvergenceSimulator:
  /**
   * @param incarnation the specified [[SimulationIncarnation]].
   * @return a new [[ConvergenceSimulator]] configuring simulations for the
   *         specified [[SimulationIncarnation]].
   */
  def apply[I <: SimulationIncarnation](incarnation: I): ConvergenceSimulator[I] =
    BasicConvergenceSimulator(incarnation)

  /** Basic implementation of [[ConvergenceSimulator]]. */
  private class BasicConvergenceSimulator[I <: SimulationIncarnation](override val incarnation: I)
    extends BasicSimulator(incarnation) with ConvergenceSimulator[I]:
    import incarnation.{*, given}

    override def computeLimit[A](flow: Flow[A])(using
      environment: Environment,
      haltPolicy: HaltPolicy[CollectiveResultMap[A]],
      executor: ExecutionContext
    ): Future[CollectiveResultMap[A]] =
      val simulation = this.simulation(flow)
      val computation = haltPolicy.apply(simulation.computedByAll)
      val computationMonitor = Stream.monitor(computation.events, memory = 1)
      val termination = computation.termination
      termination.onComplete(_ => simulation.stop())
      simulation.start()
      termination.map(_ => computationMonitor.eventLog.lastOption.collect({ case Event(p) => p }).getOrElse(Map()))
