package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.core.Incarnation
import it.unibo.distributedfrp.simulation.simulator.{BasicSimulator, Simulator, HaltPolicy}
import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.frp.FiniteStreamExtension.*
import it.unibo.distributedfrp.frp.FiniteStreamExtension.FiniteEvent.*
import it.unibo.distributedfrp.simulation.{Environment, SimulationIncarnation}

import scala.annotation.targetName
import scala.concurrent.{ExecutionContext, Future}

/**
 * A [[Simulator]] that is capable of checking the convergence of its flows.
 * @tparam I the type of [[Incarnation]] used by this [[Simulator]].
 */
trait ConvergenceSimulator[I <: Incarnation] extends Simulator[I]:
  /**
   * Check if the specified [[Flow Flow]] converges to the specified limit.
   *
   * @param flow  the specified [[Flow Flow]].
   * @param limit the specified limit.
   * @tparam A the type of result computed by the [[Flow Flow]].
   * @return a [[Future]] containing true if the specified [[Flow Flow]]
   *         converges to the specified limit, false otherwise.
   *         The [[Future]] completes when the simulation of the specified
   *         [[Flow Flow]] is completed, using the given [[HaltPolicy HaltPolicy]].
   */
  def checkConvergence[A](flow: Flow[A], limit: CollectiveResultMap[A])(using HaltPolicy[CollectiveResultMap[A]]): Future[Boolean]

  /**
   * Check if the specified [[Flow Flow]]s converges to the same limit.
   *
   * @param flows the specified [[Flow Flow]]s.
   * @tparam A the type of result computed by the [[Flow Flow]]s.
   * @return a [[Future]] containing true if the specified [[Flow Flow]]s
   *         converges to the same limit, false otherwise.
   *         The [[Future]] completes when the simulation of the specified
   *         [[Flow Flow]]s is completed, using the given [[HaltPolicy HaltPolicy]].
   */
  def checkConvergentEquivalence[A](flows: Flow[A]*)(using HaltPolicy[CollectiveResultMap[A]]): Future[Boolean]

/** Companion object of [[ConvergenceSimulator]]. */
object ConvergenceSimulator:
  /** The default [[ConvergenceSimulator]]. */
  private val Default: ConvergenceSimulator[SimulationIncarnation] =
    ConvergenceSimulator(SimulationIncarnation(Environment.manhattanGrid(3, 3)))(using ExecutionContext.global)

  /** @return the default [[ConvergenceSimulator]]. */
  def default: ConvergenceSimulator[SimulationIncarnation] = ConvergenceSimulator.Default

  /**
   * @param incarnation      the specified [[SimulationIncarnation]].
   * @param ExecutionContext the given [[ExecutionContext]].
   * @return a new [[ConvergenceSimulator]] configuring simulations for the
   *         specified [[SimulationIncarnation]].
   */
  def apply(incarnation: SimulationIncarnation)(using ExecutionContext): ConvergenceSimulator[SimulationIncarnation] =
    BasicConvergenceSimulator(incarnation)

  /** Basic implementation of [[ConvergenceSimulator]]. */
  private class BasicConvergenceSimulator(incarnation: SimulationIncarnation)(using ExecutionContext)
    extends BasicSimulator(incarnation) with ConvergenceSimulator[SimulationIncarnation]:

    override def checkConvergence[A](flow: Flow[A], limit: CollectiveResultMap[A])(using
      HaltPolicy[CollectiveResultMap[A]]
    ): Future[Boolean] =
      this.computeLimit(flow).map(_ == limit)

    override def checkConvergentEquivalence[A](flows: Flow[A]*)(using HaltPolicy[CollectiveResultMap[A]]): Future[Boolean] =
      flows
        .map(this.computeLimit)
        .foldLeft(Future.successful(Seq.empty[CollectiveResultMap[A]]))((accFuture, nextFuture) =>
          accFuture.flatMap(acc => nextFuture.map(next => acc :+ next))
        )
        .map(lasts => lasts.forall(_ == lasts.head))

    /**
     * @param flow the specified [[Flow Flow]].
     * @return a [[Future]] containing the latest value computed by all
     *         the devices running the specified [[Flow Flow]].
     *         The [[Future]] completes when the simulation of this [[Flow Flow]]
     *         is completed, using the given [[HaltPolicy HaltPolicy]].
     */
    private def computeLimit[A](flow: Flow[A])(using
      haltPolicy: HaltPolicy[CollectiveResultMap[A]]
    ): Future[CollectiveResultMap[A]] =
      val simulation = this.simulation(flow)
      val computation = haltPolicy.apply(simulation.computedByAll)
      val computationMonitor = Stream.monitor(computation, memory = 2)
      val termination = computation.termination
      termination.onComplete(_ => simulation.stop())
      simulation.start()
      termination.map(_ => computationMonitor.eventLog).map {
        case Event(result) :: EOS :: Nil => result
        case _ => Map()
      }
  end BasicConvergenceSimulator

  /**
   * A [[ConvergenceSimulator]] with operator shorthands for checking the
   * convergence of its flows.
   *
   * @tparam I the type of [[Incarnation]] used by the [[Simulator]].
   */
  trait DSL[I <: Incarnation] extends ConvergenceSimulator[I]:
    extension[A] (using HaltPolicy[CollectiveResultMap[A]])(self: Flow[A]) {
      /** Alias for [[checkConvergence checkConvergence(self, limit)]]. */
      infix def convergesTo(limit: CollectiveResultMap[A]): Future[Boolean] =
        DSL.this.checkConvergence(self, limit)

      /** Alias for [[checkConvergentEquivalence checkConvergentEquivalence(self +: others *)]]. */
      infix def isConvergentlyEquivalentTo(others: Flow[A]*): Future[Boolean] =
        DSL.this.checkConvergentEquivalence(self +: others *)

      /** Alias for [[convergesTo self convergesTo result]]. */
      @targetName("convergesToOperator")
      infix def -->(result: CollectiveResultMap[A]): Future[Boolean] =
        self convergesTo result

      /** Alias for [[isConvergentlyEquivalentTo self isConvergentlyEquivalentTo other]]. */
      @targetName("isConvergentlyEquivalentToOperator")
      infix def ~~>(other: Flow[A]): Future[Boolean] =
        self isConvergentlyEquivalentTo other
    }

  /** Companion object of [[DSL]]. */
  object DSL:
    /** The default [[DSL]]. */
    private val Default: DSL[SimulationIncarnation] =
      DSL(SimulationIncarnation(Environment.manhattanGrid(3, 3)))(using ExecutionContext.global)

    /** @return the default [[DSL]]. */
    def default: DSL[SimulationIncarnation] = DSL.Default

    /**
     * @param incarnation      the specified [[SimulationIncarnation]].
     * @param ExecutionContext the given [[ExecutionContext]].
     * @return a new [[DSL]] configuring simulations for the specified [[SimulationIncarnation]].
     */
    def apply(incarnation: SimulationIncarnation)(using ExecutionContext): DSL[SimulationIncarnation] =
      BasicDSL(incarnation)

    /** Basic implementation of [[DSL]]. */
    private class BasicDSL(incarnation: SimulationIncarnation)(using ExecutionContext)
      extends BasicConvergenceSimulator(incarnation) with DSL[SimulationIncarnation]
