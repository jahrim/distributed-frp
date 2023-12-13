package it.unibo.distributedfrp.core.convergence

import it.unibo.distributedfrp.core.Incarnation
import it.unibo.distributedfrp.simulation.simulator.HaltPolicy
import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.frp.FiniteStreamExtension.*
import it.unibo.distributedfrp.test.utils.AbstractTest

import scala.concurrent.Await
import scala.concurrent.duration.*

/** An [[AbstractTest]] for checking the convergence of flows. */
trait ConvergenceTest extends AbstractTest:
  /** The default timeout when awaiting the end of the simulations. */
  protected def defaultTimeout: Duration = 30.seconds

  /** The default [[HaltPolicy]] for terminating the simulations. */
  protected given defaultHaltPolicy[T]: HaltPolicy[T] = HaltPolicy.haltAfterInactivityOf(200.milliseconds)

  /**
   * Create a convergence test for the specified [[Flow Flow]].
   * The test succeeds if the specified [[Flow Flow]] converges to
   * the specified limit, using [[checkConvergence]].
   *
   * @param simulator   the [[ConvergenceSimulator]] used to run the test.
   * @param flow        the specified [[Flow Flow]].
   * @param limit       the specified limit.
   * @param expectation if false, the test succeeds when the specified [[Flow Flow]]
   *                    does NOT converge to the specified limit, instead.
   * @tparam A the type of the results computed by the specified [[Flow Flow]].
   */
  def convergenceTest[A, I <: Incarnation, S <: ConvergenceSimulator[I]](
    simulator: S,
    flow: simulator.Flow[A],
    limit: simulator.CollectiveResultMap[A],
    expectation: Boolean = true,
  )(using HaltPolicy[simulator.CollectiveResultMap[A]]): Unit =
    Await.result(simulator.checkConvergence(flow, limit), defaultTimeout) shouldBe expectation

  /**
   * Create a convergence test for the specified [[Flow Flow]]s.
   * The test succeeds if the specified [[Flow Flow]]s converges to
   * the same limit, using [[checkConvergence]].
   *
   * @param simulator   the [[ConvergenceSimulator]] used to run the test.
   * @param flows       the specified [[Flow Flow]]s.
   * @param expectation if false, the test succeeds when the specified [[Flow Flow]]s
   *                    do NOT converge to the same limit, instead.
   * @tparam A the type of the results computed by the specified [[Flow Flow]]s.
   */
  def convergentEquivalenceTest[A, I <: Incarnation, S <: ConvergenceSimulator[I]](
    simulator: S,
    flows: Seq[simulator.Flow[A]],
    expectation: Boolean = true,
  )(using HaltPolicy[simulator.CollectiveResultMap[A]]): Unit =
    Await.result(simulator.checkConvergentEquivalence(flows *), defaultTimeout) shouldBe expectation
