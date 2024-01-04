package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.core.FraspTest
import it.unibo.distributedfrp.simulation.simulator.{ConvergenceSimulator, HaltPolicy}
import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.frp.FiniteStreamExtension.*
import it.unibo.distributedfrp.simulation.environment.{Environment, EnvironmentFactory, EnvironmentWithTags}
import it.unibo.distributedfrp.simulation.incarnation.{CommonAlgorithms, CommonSensors, SimulationIncarnation}
import org.scalactic.Prettifier

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

/** An [[AbstractTest]] for checking the convergence of flows. */
trait ConvergenceTest extends FraspTest:
  /**
   * The [[Expectation Expectation]] for a [[ConvergenceTest]]. It can be either:
   *  - [[Expectation.Convergent]]: the test will succeed if the convergence hold.
   *  - [[Expectation.Divergent]]: the test will succeed if the convergence does not hold.
   */
  protected enum Expectation { case Convergent, Divergent }

  /** The default timeout when awaiting the convergence of flows. */
  protected def defaultTimeout: Duration
  /** The default number of repetitions of each test. */
  protected def defaultRepetitions: Int

  /**
   * Create a convergence test for the specified [[Flow Flow]].
   *
   * The test checks if the specified [[Flow Flow]] converges to the specified
   * limit, succeeding depending on the specified [[Expectation Expectation]].
   *
   * @param simulator   the [[ConvergenceSimulator]] used to run the test.
   * @param flow        the specified [[Flow Flow]].
   * @param limit       the specified limit.
   * @param expectation the specified [[Expectation Expectation]].
   * @param repetitions the amount of concurrent simulations that should be
   *                    executed and for which the test should succeed.
   * @tparam A the type of the results computed by the specified [[Flow Flow]].
   */
  protected def convergenceTest[A, I <: SimulationIncarnation, S <: ConvergenceSimulator[I]](
    simulator: S,
    flow: simulator.incarnation.Environment ?=> simulator.incarnation.Flow[A],
    limit: simulator.CollectiveResultMap[A],
    expectation: Expectation = Expectation.Convergent,
    repetitions: Int = defaultRepetitions,
  )(using
    environmentFactory: EnvironmentFactory[simulator.incarnation.Environment],
    haltPolicy: HaltPolicy[simulator.CollectiveResultMap[A]],
    executor: ExecutionContext,
    testFormatter: Prettifier
  ): Unit =
    repeatTest(repetitions){
      given simulator.incarnation.Environment = environmentFactory.newInstance()
      simulator.computeLimit(flow).map(actualLimit =>
        checkConvergence(actualLimit, limit, expectation)
      )
    }

  /**
   * Create a convergence test for the specified [[Flow Flow]]s.
   *
   * The test checks if the specified [[Flow Flow]]s converge to the same limit,
   * succeeding depending on the specified [[Expectation Expectation]].
   *
   * @param simulator   the [[ConvergenceSimulator]] used to run the test.
   * @param flows       the specified [[Flow Flow]]s.
   * @param expectation the specified [[Expectation Expectation]].
   * @param repetitions the amount of concurrent simulations that should be
   *                    executed and for which the test should succeed.
   * @tparam A the type of the results computed by the specified [[Flow Flow]]s.
   */
  protected def convergentEquivalenceTest[A, I <: SimulationIncarnation, S <: ConvergenceSimulator[I]](
    simulator: S,
    flows: Seq[simulator.incarnation.Environment ?=> simulator.incarnation.Flow[A]],
    expectation: Expectation = Expectation.Convergent,
    repetitions: Int = defaultRepetitions,
  )(using
    environmentFactory: EnvironmentFactory[simulator.incarnation.Environment],
    haltPolicy: HaltPolicy[simulator.CollectiveResultMap[A]],
    executor: ExecutionContext,
    testFormatter: Prettifier
  ): Unit =
    repeatTest(repetitions) {
      simulator.computeLimits(flows *).map(limits =>
        limits.headOption.foreach { expectedLimit =>
          forEvery(limits.zipWithIndex) { (actualLimit, index) =>
            if index > 0 then checkConvergence(actualLimit, expectedLimit, expectation)
          }
        }
      )
    }

  /**
   * Repeat the specified test for the specified number of repetitions.
   *
   * @param repetitions the specified number of repetitions.
   * @param test        the specified test.
   */
  private def repeatTest(repetitions: Int)(test: => Future[Unit])(using ExecutionContext): Unit =
    Await.result(Future.sequence(Seq.range(0, repetitions).map(_ => test)), defaultTimeout)

  /**
   * Create a test that checks if the specified actual limit is the same as the
   * specified expected limit, succeeding depending on the specified [[Expectation]].
   *
   * @param actualLimit   the specified actual limit.
   * @param expectedLimit the specified expected limit.
   * @param expectation   the specified [[Expectation]].
   * @tparam L the type of the specified limits.
   */
  private def checkConvergence[L](actualLimit: L, expectedLimit: L, expectation: Expectation)(using Prettifier): Unit =
    if expectation == Expectation.Convergent
    then actualLimit should equal(expectedLimit)
    else actualLimit shouldNot equal(expectedLimit)

/** Companion object of [[ConvergenceTest]]. */
object ConvergenceTest:
  /** The default configuration of a [[ConvergenceTest]]. */
  trait WithDefaults extends ConvergenceTest:
    override protected def defaultTimeout: Duration = 60.seconds
    override protected def defaultRepetitions: Int = 1
    protected given defaultHaltPolicy[T]: HaltPolicy[T] = HaltPolicy.haltAfterInactivityOf(500.milliseconds)
    protected given defaultEnvironmentFactory: EnvironmentFactory[EnvironmentWithTags] = () =>
      EnvironmentWithTags(Environment.euclideanGrid(cols = 3, rows = 3))
    protected given defaultPrettifier: Prettifier = _ match
      case map: Map[?, ?] if map.keys.forall(_.isInstanceOf[Int]) =>
        val typedMap: Map[Int, ?] = map.asInstanceOf[Map[Int, ?]]
        s"[\n\t${typedMap.toSeq.sortBy(_._1).mkString("\n\t")}\n]"
      case any => Prettifier.default(any)
    protected object DefaultIncarnation extends SimulationIncarnation with CommonSensors.Default with CommonAlgorithms
    protected val defaultSimulator: ConvergenceSimulator[DefaultIncarnation.type] =
      ConvergenceSimulator(DefaultIncarnation)
