package it.unibo.distributedfrp.test.utils.logic

import it.unibo.distributedfrp.test.utils.AbstractTest
import it.unibo.distributedfrp.test.utils.logic.Logic

/**
 * A test for a [[Logic]].
 *
 * A [[LogicTest]] defines a set of formulas that should hold and a set of
 * evaluators used to evaluate the validity of such formulas. Moreover, it
 * provides functionalities to define custom tests on formulas.
 *
 * @tparam S the type of the subject of the formulas.
 * @tparam L the type of [[Logic]] to be tested.
 */
abstract class LogicTest[L <: Logic, S] extends AbstractTest:
  /** @return the [[Logic]] to be tested. */
  protected val logic: L
  import logic.*

  /** The unique name of a [[Formula Formula]] to be tested. */
  protected type FormulaId = String
  /** The unique name of an [[Evaluator Evaluator]] to use for testing. */
  protected type EvaluatorId = String

  /**
   * @return a [[Map]] from [[FormulaId FormulaId]]s to the corresponding
   *         [[Formula Formula]]s that will be verified during this [[LogicTest]].
   */
  protected def formulas: Map[FormulaId, Formula[S]]

  /**
   * @return a [[Map]] from [[EvaluatorId EvaluatorId]]s to the corresponding
   *         [[Evaluator Evaluator]]s used to evaluate the validity of the
   *         specified [[Formula Formula]]s.
   */
  protected def evaluators: Map[EvaluatorId, Evaluator[S]]

  /**
   * @return a sequence of the combinations of [[Formula Formula]] and
   *         [[Evaluator Evaluator]] identifiers that should not be tested.
   */
  protected def excluded: Seq[(FormulaId, EvaluatorId)] = Seq()

  /** Verify the [[Formula Formula]]s of this [[LogicTest]]. */
  protected def verifyRegisteredFormulas(): Unit =
    formulas.foreach { (formulaId, formula) =>
      behavior of formulaId
      evaluators.foreach { (evaluatorId, evaluator) =>
        if !excluded.contains((formulaId, evaluatorId)) then
          it should s"hold for evaluator $evaluatorId" in {
            evaluator(formula).isSatisfied shouldBe true
          }
      }
    }

  /**
   * Create a custom general test for the [[Logic]] of this [[LogicTest]].
   * The test succeeds if applying the specified [[Evaluator Evaluator]] to
   * the specified [[Formula Formula]]s yields the specified expectations.
   *
   * @param evaluator    the specified [[Evaluator Evaluator]]. Defaults to
   *                     [[evaluators evaluators.head]].
   * @param expectations the specified expectations bound to the corresponding
   *                     [[Formula Formula]]s.
   */
  protected def testFormulas(
    evaluator: Evaluator[S] =
    evaluators.values.headOption.getOrElse(
      throw IllegalStateException("Could not find a default evaluator: no evaluators registered.")
    ),
    expectations: Seq[(Evaluation, Formula[S])] = Seq(),
  ): Unit =
    expectations
      .map(_ -> evaluator(_))
      .zipWithIndex
      .collect { case ((expected, actual), index) if expected != actual => (expected, actual, index) }
      .map((expected, actual, index) => s"Formula $index expected to be $expected was $actual.")
      .reduceOption(_ + '\n' + _)
      .foreach(message => fail(s"\n$message"))

  /**
   * Create a sequence of expectations for the specified [[Formula Formula]]s,
   * where all [[Formula Formula]]s are expected to yield the same specified
   * [[Evaluation Evaluation]].
   *
   * @param expectation the specified [[Evaluation Evaluation]].
   * @param fs          the specified [[Formula Formula]]s.
   * @return a sequence of expectations that can be used within [[testFormulas]].
   */
  protected def expectAll(expectation: Evaluation)(fs: Formula[Int]*): Seq[(Evaluation, Formula[Int])] =
    fs.map(expectation -> _)
