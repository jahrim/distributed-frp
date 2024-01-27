package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.AbstractTest
import it.unibo.distributedfrp.core.FraspSamples
import it.unibo.distributedfrp.simulation.environment.{Environment, EnvironmentWithTags}
import it.unibo.distributedfrp.simulation.incarnation.{CommonAlgorithms, CommonSensors, SimulationIncarnation}
import it.unibo.distributedfrp.simulation.simulator.Simulator
import it.unibo.distributedfrp.simulation.simulator.Simulator.WithIncarnation
import it.unibo.distributedfrp.simulation.simulator.convergence.ConvergenceSimulator
import it.unibo.distributedfrp.utils.Logger
import org.scalactic.Prettifier

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

/** An [[AbstractTest]] for checking the convergence of flows. */
trait ConvergenceTest extends AbstractTest:
  /** The type of a single unit test. */
  type Test = Prettifier ?=> Unit

  /**
   * The [[Expectation Expectation]] for a [[ConvergenceTest]]. It can be either:
   *  - [[Expectation.Convergent]]: the test will succeed if the convergence hold.
   *  - [[Expectation.Divergent]]: the test will succeed if the convergence does not hold.
   */
  protected enum Expectation { case Convergent, Divergent }

  /** The default number of repetitions of each test. */
  protected def defaultRepetitions: Int = 1

  /**
   * Create a convergence test for the specified [[Flow Flow]].
   *
   * The test checks if the specified [[Flow Flow]] converges to the specified
   * limit before the specified timeout, succeeding depending on the specified
   * [[Expectation Expectation]].
   *
   * @param simulator             the [[ConvergenceSimulator]] used to run the test.
   * @param flow                  the specified [[Flow Flow]].
   * @param limit                 the specified limit.
   * @param expectation           the specified [[Expectation Expectation]].
   * @param repetitions           the amount of concurrent simulations that should be
   *                              executed and for which the test should succeed.
   * @param timeout               the specified timeout.
   * @param executor              the [[ExecutionContext]] used for executing the
   *                              repetitions of the test concurrently.
   * @param configurationSupplier the [[ConfigurationSupplier ConfigurationSupplier]]
   *                              providing the configuration for the simulation of
   *                              each repetition.
   * @tparam A the type of the results computed by the specified [[Flow Flow]].
   */
  protected def convergenceTest[A, S <: Simulator with ConvergenceSimulator](
    simulator: S,
    flow: simulator.Configuration[A] ?=> simulator.incarnation.Flow[A],
    limit: simulator.Configuration[A] ?=> simulator.CollectiveResultMap[A],
    expectation: Expectation = Expectation.Convergent,
    repetitions: Int = defaultRepetitions,
    timeout: Duration = Defaults.timeout,
  )(using
    executor: ExecutionContext = Defaults.executor,
    configurationSupplier: () => simulator.Configuration[A]
  ): Test =
    repeatTest(repetitions, timeout){
      Future.delegate {
        given simulator.Configuration[A] = configurationSupplier()
        simulator.computeLimit(flow).map(actualLimit => checkConvergence(actualLimit, limit, expectation))
      }
    }

  /**
   * Create a convergence test for the specified [[Flow Flow]]s.
   *
   * The test checks if the specified [[Flow Flow]]s converge to the same limit
   * before the specified timeout, succeeding depending on the specified
   * [[Expectation Expectation]].
   *
   * @param simulator             the [[ConvergenceSimulator]] used to run the test.
   * @param flows                 the specified [[Flow Flow]]s.
   * @param expectation           the specified [[Expectation Expectation]].
   * @param repetitions           the amount of concurrent simulations that should be
   *                              executed and for which the test should succeed.
   * @param timeout               the specified timeout.
   * @param executor              the [[ExecutionContext]] used for executing the
   *                              repetitions of the test concurrently.
   * @param configurationSupplier the [[ConfigurationSupplier ConfigurationSupplier]]
   *                              providing the configuration for the simulation of
   *                              each [[Flow Flow]] and repetition.
   * @tparam A the type of the results computed by the specified [[Flow Flow]]s.
   */
  protected def convergentEquivalenceTest[A, S <: Simulator with ConvergenceSimulator](
    simulator: S,
    flows: Seq[simulator.Configuration[A] ?=> simulator.incarnation.Flow[A]],
    expectation: Expectation = Expectation.Convergent,
    repetitions: Int = defaultRepetitions,
    timeout: Duration = Defaults.timeout,
  )(using
    executor: ExecutionContext = Defaults.executor,
    configurationSupplier: () => simulator.Configuration[A]
  ): Test =
    repeatTest(repetitions, timeout) {
      Future.delegate {
        simulator.computeLimits(flows *).map(limits =>
          limits.headOption.foreach { expectedLimit =>
            forEvery(limits.zipWithIndex) { (actualLimit, index) =>
              if index > 0 then checkConvergence(actualLimit, expectedLimit, expectation)
            }
          }
        )
      }
    }

  /**
   * Create a new test that repeats the specified test for the specified number
   * of repetitions.
   *
   * The new test succeeds only if all the repetitions of the specified test
   * succeed before the specified timeout [[Duration]], failing otherwise.
   *
   * @param repetitions the specified number of repetitions.
   * @param timeout     the specified timeout.
   * @param test        the specified test.
   */
  private def repeatTest(repetitions: Int, timeout: Duration)(test: => Future[?])(using ExecutionContext): Test =
    Await.result(Future.sequence(Seq.range(0, repetitions).map(_ => test)), timeout)

  /**
   * Create a test that checks if the specified actual limit is the same as the
   * specified expected limit, succeeding depending on the specified [[Expectation]].
   *
   * @param actualLimit   the specified actual limit.
   * @param expectedLimit the specified expected limit.
   * @param expectation   the specified [[Expectation]].
   * @tparam L the type of the specified limits.
   */
  private def checkConvergence[L](actualLimit: L, expectedLimit: L, expectation: Expectation): Test =
    if expectation == Expectation.Convergent
    then actualLimit should equal(expectedLimit)
    else actualLimit shouldNot equal(expectedLimit)

/** Companion object of [[ConvergenceTest]]. */
object ConvergenceTest:
  /** A collection of the default configurations of a [[ConvergenceTest]]. */
  object Defaults:
    /** The default [[Prettifier]] for a [[ConvergenceTest]]. */
    val DefaultPrettifier: Prettifier = _ match
      case map: Map[?, ?] if map.keys.forall(_.isInstanceOf[Int]) =>
        s"[\n\t${map.asInstanceOf[Map[Int, ?]].toSeq.sortBy(_._1).mkString("\n\t")}\n]"
      case any => Prettifier.default(any)

    /**
     * The default configuration of a [[ConvergenceTest]] with a
     * [[StepConvergenceSimulator StepConvergenceSimulator]].
     */
    trait WithStepSimulator extends ConvergenceTest:
      override protected def defaultRepetitions: Int = 1
      protected given defaultPrettifier: Prettifier = ConvergenceTest.Defaults.DefaultPrettifier
      protected object DefaultIncarnation
        extends SimulationIncarnation with CommonSensors.Default with CommonAlgorithms with FraspSamples
      protected object DefaultSimulator
        extends ConvergenceSimulator.StepSimulator with WithIncarnation(DefaultIncarnation):
        override type Configuration[A] = StepSimulationConfiguration[A]
      protected def defaultEnvironment: EnvironmentWithTags =
        EnvironmentWithTags(Environment.euclideanGrid(cols = 3, rows = 3))
      protected def defaultHaltPolicy[T]: DefaultSimulator.HaltPolicy[T] =
        DefaultSimulator.HaltPolicy.haltOnVainStep
      protected given defaultSimulationConfigurationProvider[A]: (() => DefaultSimulator.Configuration[A]) =
        val simulationIds: Iterator[Int] = LazyList.iterate(0)(_ + 1).iterator
        () => DefaultSimulator.StepSimulationConfiguration(
          environment = defaultEnvironment,
          haltPolicy = defaultHaltPolicy,
          logger = Logger.named(
            name = "StepConvergenceSimulator#" + simulationIds.synchronized { simulationIds.next() },
            verbosity = 0
          )
        )

      /** @return the [[Environment Environment]] of the implicit [[SimulationConfiguration]]. */
      protected def environment(using DefaultSimulator.Configuration[?]): DefaultSimulator.incarnation.Environment =
        summon[DefaultSimulator.Configuration[?]].environment
