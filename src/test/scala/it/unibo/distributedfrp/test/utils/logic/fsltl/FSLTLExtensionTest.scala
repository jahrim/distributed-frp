package it.unibo.distributedfrp.test.utils.logic.fsltl

import it.unibo.distributedfrp.frp.FiniteStreamExtension.*
import it.unibo.distributedfrp.frp.StreamSample.*
import it.unibo.distributedfrp.frp.StreamSample
import it.unibo.distributedfrp.test.utils.logic.fol.FirstOrderLogic
import it.unibo.distributedfrp.test.utils.logic.fsltl.FSLTLExtension.*
import it.unibo.distributedfrp.test.utils.logic.fsltl.FSLTLExtensionTest.CustomLTL
import it.unibo.distributedfrp.test.utils.logic.propositional.{PropositionalLogic, PropositionalLogicTest}
import it.unibo.distributedfrp.test.utils.logic.temporal.{LinearTemporalLogic, LinearTemporalLogicTest}
import it.unibo.distributedfrp.test.utils.logic.trilean.{KleeneLogic, PriestLogic}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

/** Companion object of [[FiniteStreamLTLExtensionTest]]. */
object FSLTLExtensionTest:
  private given ExecutionContext = ExecutionContext.global

  /** A customized [[FSLTL FSLTL]]. */
  object CustomLTL extends FSLTL
    with FSLTL.DSL
    with FSLTL.Shorthands
    with PriestLogic
    with FirstOrderLogic.Comparisons:
    val zero: Formula[Int] = is[Int](0)
    val nonZero: Formula[Int] = not(zero)
    val even: Formula[Int] = let[Int](x => ?(x % 2 == 0))
    val odd: Formula[Int] = not(even)

/** Test for [[FSLTLExtension]]. */
class FSLTLExtensionTest extends LinearTemporalLogicTest[CustomLTL.type, Int]:
  private val FiniteStreamEvaluator = symbol("FiniteStreamEvaluator")
  private val EmptyStreamEvaluator = symbol("EmptyStreamEvaluator")

  private val Verum = symbol("verum")
  private val Falsum = symbol("falsum")
  private val Ignotum = symbol("ignotum")
  private val Proposition = symbol("proposition")
  private val Not = symbol("not")
  private val And = symbol("and")
  private val Or = symbol("or")
  private val Conditional = symbol("conditional")
  private val Biconditional = symbol("biconditional")
  private val Predicate = symbol("Predicate")
  private val Next = symbol("next")
  private val Until = symbol("until")
  private val Sometimes = symbol("sometimes")
  private val Always = symbol("always")
  private val WeakUntil = symbol("weakUntil")

  override protected val logic: CustomLTL.type = CustomLTL
  given logic.type = logic
  import logic.*

  override protected def atoms: Seq[Formula[Int]] = Seq(verum, falsum, ignotum)

  private val finiteStreamEvaluator: Evaluator[Int] = streamEvaluator(finiteStreamSample)
  private val emptyStreamEvaluator: Evaluator[Int] = streamEvaluator(emptyFiniteStreamSample)
  private val infiniteStreamEvaluator: Evaluator[Int] = streamEvaluator(infiniteFiniteStreamSample)
  override protected def evaluators: Map[String, Evaluator[Int]] = Map(
    FiniteStreamEvaluator -> finiteStreamEvaluator,
    EmptyStreamEvaluator -> emptyStreamEvaluator,
  )

  override protected def formulas: Map[String, Formula[Int]] = rulesOfInference ++ rulesOfReplacement

  /**
   * @param sample the specified [[StreamSample]].
   * @return a new [[Evaluator Evaluator]] for [[Formula Formula]]s that
   *         checks the [[Formula Formula]]s against the [[FiniteStream FiniteStream]]
   *         of the specified [[StreamSample]].
   */
  private def streamEvaluator(sample: StreamSample[FiniteStream[Int]]): Formula[Int] => Evaluation = formula =>
    val formulaEvaluation: AsyncEvaluation = sample.streams.check(formula)
    sample.generateEvents()
    Await.result(formulaEvaluation, 30.seconds)

  verifyRegisteredFormulas()

  Verum should "always be satisfied" in testFormulas(
    expectations = expectAll(True)(verum, T),
  )
  it should "always be satisfied for an infinite stream" in testFormulas(
    evaluator = infiniteStreamEvaluator,
    expectations = expectAll(True)(verum, T),
  )
  it should "always be satisfied for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(verum, T)
  )

  Falsum should "always be falsified" in testFormulas(
    expectations = expectAll(False)(falsum, F)
  )
  it should "always be falsified for an infinite stream" in testFormulas(
    evaluator = infiniteStreamEvaluator,
    expectations = expectAll(False)(falsum, F)
  )
  it should "always be unknown for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(falsum, F)
  )

  Ignotum should "always be unknown" in testFormulas(
    expectations = expectAll(Unknown)(ignotum, U)
  )
  it should "always be unknown for an infinite stream" in testFormulas(
    evaluator = infiniteStreamEvaluator,
    expectations = expectAll(Unknown)(ignotum, U)
  )
  it should "always be unknown for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(ignotum, U)
  )

  Proposition should "be satisfied if it yields a True truth value" in testFormulas(
    expectations = expectAll(True)(
      proposition(True), ?(True),
      proposition(true), ?(true),
    )
  )
  it should "be falsified if it yields a False truth value" in testFormulas(
    expectations = expectAll(False)(
      proposition(False), ?(False),
      proposition(false), ?(false),
    )
  )
  it should "be unknown if it yields an Unknown truth value" in testFormulas(
    expectations = expectAll(Unknown)(
      proposition(Unknown), ?(Unknown),
    )
  )
  it should "always be determined for an infinite stream" in testFormulas(
    evaluator = infiniteStreamEvaluator,
    expectations = (
      expectAll(True)(
        proposition(True), ?(True),
        proposition(true), ?(true),
      ) ++
      expectAll(False)(
        proposition(False), ?(False),
        proposition(false), ?(false),
      ) ++
      expectAll(Unknown)(proposition(Unknown), ?(Unknown))
    )
  )
  it should "always be unknown for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(
      proposition(True), ?(True),
      proposition(true), ?(true),
      proposition(False), ?(False),
      proposition(false), ?(false),
      proposition(Unknown), ?(Unknown),
    ),
  )

  Not should "be satisfied if its formula is falsified" in testFormulas(
    // logic.not disambiguates from org.scalatest.matchers.should.Matchers.not
    expectations = expectAll(True)(logic.not(F), !F),
  )
  it should "be falsified if its formula is satisfied" in testFormulas(
    expectations = expectAll(False)(logic.not(T), !T),
  )

  And should "be satisfied if both of its formulas are satisfied" in testFormulas(
    expectations = expectAll(True)(
      and(T, T), T and T, T && T
    )
  )
  it should "be falsified if any of its formulas is falsified" in testFormulas(
    expectations = expectAll(False)(
      and(T, F), T and F, T && F,
      and(F, T), F and T, F && T,
      and(F, F), F and F, F && F,
    )
  )

  Or should "be satisfied if any of its formulas is satisfied" in testFormulas(
    expectations = expectAll(True)(
      or(T, T), T or T, T || T,
      or(T, F), T or F, T || F,
      or(F, T), F or T, F || T,
    )
  )
  it should "be falsified if both of its formulas are falsified" in testFormulas(
    expectations = expectAll(False)(
      or(F, F), F or F, F || F,
    )
  )

  Conditional should "be satisfied if its first formula implies its second formula" in testFormulas(
    expectations = expectAll(True)(
      conditional(T, T), T implies T, T ---> T,
      conditional(F, T), F implies T, F ---> T,
      conditional(F, F), F implies F, F ---> F,
    )
  )
  it should "be falsified if its first formula does not imply its second formula" in testFormulas(
    expectations = expectAll(False)(
      conditional(T, F), T implies F, T ---> F,
    )
  )

  Biconditional should "be satisfied if its formulas are logically equivalent" in testFormulas(
    expectations = expectAll(True)(
      biconditional(T, T), T conforms T, T <--> T,
      biconditional(F, F), F conforms F, F <--> F,
    )
  )
  it should "be falsified if its formulas are not logically equivalent" in testFormulas(
    expectations = expectAll(False)(
      biconditional(T, F), T conforms F, T <--> F,
      biconditional(F, T), F conforms T, F <--> T,
    )
  )

  Predicate should "be satisfied if its formula is satisfied in the current state of the system" in testFormulas(
    expectations = expectAll(True)(
      predicate[Int](x => T), let[Int](x => T), P[Int](x => T),
      predicate[Int](x => ?(x == 0)), let[Int](x => ?(x == 0)), P[Int](x => ?(x == 0)),
    )
  )
  it should "be falsified if its formula is falsified in the current state of the system" in testFormulas(
    expectations = expectAll(False)(
      predicate[Int](x => F), let[Int](x => F), P[Int](x => F),
      predicate[Int](x => ?(x != 0)), let[Int](x => ?(x != 0)), P[Int](x => ?(x != 0)),
    )
  )
  it should "always be unknown for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(
      predicate[Int](x => T), let[Int](x => T), P[Int](x => T),
      predicate[Int](x => ?(x == 0)), let[Int](x => ?(x == 0)), P[Int](x => ?(x == 0)),
      predicate[Int](x => F), let[Int](x => F), P[Int](x => F),
      predicate[Int](x => ?(x != 0)), let[Int](x => ?(x != 0)), P[Int](x => ?(x != 0)),
    )
  )
  it should "combine correctly with other operators" in testFormulas(
    expectations =
      expectAll(True)(
        predicate[Int](x => next(predicate[Int](y => ?(x != y)))),
        let[Int](x => next(let[Int](y => ?(x != y)))),
        P[Int](x => X(P[Int](y => ?(x != y)))),
        always(predicate[Int](x => next(predicate[Int](y => ?(y == x + 1))))),
        always(let[Int](x => next(let[Int](y => ?(y == x + 1))))),
        G(P[Int](x => next(P[Int](y => ?(y == x + 1))))),
      ) ++
      expectAll(False)(
        predicate[Int](x => next(predicate[Int](y => ?(x == y)))),
        let[Int](x => next(let[Int](y => ?(x == y)))),
        P[Int](x => X(P[Int](y => ?(x == y)))),
        sometimes(predicate[Int](x => next(predicate[Int](y => ?(x > y))))),
        sometimes(let[Int](x => next(let[Int](y => ?(x > y))))),
        F(P[Int](x => X(P[Int](y => ?(x > y))))),
      )
  )

  Next should "be satisfied if its formula is satisfied in the next state" in testFormulas(
    expectations = expectAll(True)(
      next(T), X(T),
      next(is(1)), X(is(1)),
      next(next(is(2))), X(X(is(2))),
      next(next(next(is(3)))), X(X(X(is(3)))),
    )
  )
  it should "be falsified if its formula is falsified in the next state" in testFormulas(
    expectations = expectAll(False)(
      next(F), X(F),
      next(isnt(1)), X(isnt(1)),
      next(next(isnt(2))), X(X(isnt(2))),
      next(next(next(isnt(3)))), X(X(X(isnt(3)))),
    )
  )
  it should "be unknown if the next state does not exist" in testFormulas(
    expectations = expectAll(Unknown)(
      next(next(next(next(is(4))))), X(X(X(X(is(4))))),
    )
  )
  it should "always be unknown for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(
      next(!T), X(T),
      next(F), X(F),
      next(is(1)), X(is(1)),
      next(isnt(1)), X(isnt(1)),
    )
  )

  Until should
    "be satisfied if its first formula is satisfied at least in every state before " +
    "its second formula is satisfied and its second formula is eventually satisfied" in testFormulas(
    expectations = expectAll(True)(
      until(gteq(0), is(3)), gteq(0) until is(3), gteq(0) U is(3),
    )
  )
  it should
    "be satisfied if its second formula is satisfied in the current state, " +
    "regardless of the validity of its first formula" in testFormulas(
    expectations = expectAll(True)(
      until(T, T), T until T, T U T,
      until(F, T), F until T, F U T,
      until(gteq(0), is(0)), gteq(0) until is(0), gteq(0) U is(0),
      until(lt(0), is(0)), lt(0) until is(0), lt(0) U is(0),
    )
  )
  it should
    "be falsified if its first formula is never satisfied before " +
    "its second formula is satisfied" in testFormulas(
    expectations = expectAll(False)(
      until(F, is(3)), F until is(3), F U is(3),
      until(lt(0), is(3)), lt(0) until is(3), lt(0) U is(3),
    )
  )
  it should "be falsified if its second formula is never satisfied" in testFormulas(
    expectations = expectAll(False)(
      until(T, F), T until F, T U F,
      until(F, F), F until F, F U F,
      until(gteq(0), is(4)), gteq(0) until is(4), gteq(0) U is(4)
    )
  )
  it should "always be unknown for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(
      until(T, T), T until T, T U T,
      until(F, T), F until T, F U T,
      until(T, F), T until F, T U F,
      until(F, F), F until F, F U F,
      until(gteq(0), is(4)), gteq(0) until is(4), gteq(0) U is(4),
      until(gteq(0), is(3)), gteq(0) until is(3), gteq(0) U is(3),
      until(gteq(0), is(0)), gteq(0) until is(0), gteq(0) U is(0),
      until(lt(0), is(0)), lt(0) until is(0), lt(0) U is(0),
      until(lt(0), is(3)), lt(0) until is(3), lt(0) U is(3),
      until(F, is(3)), F until is(3), F U is(3),
    )
  )

  Sometimes should "be satisfied if its formula is satisfied in any current or future state" in testFormulas(
    expectations = expectAll(True)(
      sometimes(T), F(T),
      sometimes(is(0)), F(is(0)),
      sometimes(is(3)), F(is(3)),
    )
  )
  it should "be falsified if its formula is falsified in all current and future states" in testFormulas(
    expectations = expectAll(False)(
      sometimes(F), F(F),
      sometimes(is(4)), F(is(4)),
    )
  )
  it should "always be unknown for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(
      sometimes(T), F(T),
      sometimes(is(0)), F(is(0)),
      sometimes(is(3)), F(is(3)),
      sometimes(F), F(F),
      sometimes(is(4)), F(is(4)),
    )
  )

  Always should "be satisfied if its formula is satisfied in all current and future states" in testFormulas(
    expectations = expectAll(True)(
      always(T), G(T),
      always(gteq(0)), G(gteq(0)),
      always((even implies next(odd)) and (odd implies next(even))), G((even ---> X(odd)) && (odd ---> X(even))),
    )
  )
  it should "be falsified if its formula is falsified in any current or future state" in testFormulas(
    expectations = expectAll(False)(
      always(F), G(F),
      always(lt(0)), G(lt(0)),
      always(is(2) implies next(is(2))), G(is(2) ---> X(is(2))),
    )
  )
  it should "always be unknown for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(
      always(T), G(T),
      always(F), G(F),
      always(gteq(0)), G(gteq(0)),
      always(lt(0)), G(lt(0)),
      always((even implies next(odd)) and (odd implies next(even))), G((even ---> X(odd)) && (odd ---> X(even))),
      always(is(2) implies next(is(2))), G(is(2) ---> X(is(2))),
    )
  )

  WeakUntil should
    "be satisfied if its first formula is satisfied at least in every state before " +
    "its second formula is satisfied and its second formula is eventually satisfied" in testFormulas(
    expectations = expectAll(True)(
      weakUntil(gteq(0), is(3)), gteq(0) weakUntil is(3), gteq(0) W is(3),
    )
  )
  it should
    "be satisfied if its first formula is always satisfied, even if its second formula " +
    "is never satisfied" in testFormulas(
    expectations = expectAll(True)(
      weakUntil(T, F), T weakUntil F, T W F,
      weakUntil(gteq(0), is(4)), gteq(0) weakUntil is(4), gteq(0) W is(4),
    )
  )
  it should
    "be satisfied if its second formula is satisfied in the current state, " +
    "regardless of the validity of its first formula" in testFormulas(
    expectations = expectAll(True)(
      weakUntil(T, T), T weakUntil T, T W T,
      weakUntil(F, T), F weakUntil T, F W T,
      weakUntil(gteq(0), is(0)), gteq(0) weakUntil is(0), gteq(0) W is(0),
      weakUntil(lt(0), is(0)), lt(0) weakUntil is(0), lt(0) W is(0),
    )
  )
  it should "be falsified if both its first formulas are never satisfied" in testFormulas(
    expectations = expectAll(False)(
      weakUntil(F, F), F weakUntil F, F U F,
      weakUntil(lt(0), is(4)), lt(0) weakUntil is(4), lt(0) W is(4),
    )
  )
  it should
    "be falsified if its first formula is never satisfied before " +
    "its second formula is satisfied" in testFormulas(
    expectations = expectAll(False)(
      weakUntil(F, is(3)), F weakUntil is(3), F W is(3),
      weakUntil(lt(0), is(3)), lt(0) weakUntil is(3), lt(0) W is(3),
    )
  )
  it should "always be unknown for an empty stream" in testFormulas(
    evaluator = emptyStreamEvaluator,
    expectations = expectAll(Unknown)(
      weakUntil(T, T), T weakUntil T, T W T,
      weakUntil(F, T), F weakUntil T, F W T,
      weakUntil(T, F), T weakUntil F, T W F,
      weakUntil(F, F), F weakUntil F, F U F,
      weakUntil(gteq(0), is(4)), gteq(0) weakUntil is(4), gteq(0) W is(4),
      weakUntil(gteq(0), is(3)), gteq(0) weakUntil is(3), gteq(0) W is(3),
      weakUntil(gteq(0), is(0)), gteq(0) weakUntil is(0), gteq(0) W is(0),
      weakUntil(lt(0), is(4)), lt(0) weakUntil is(4), lt(0) W is(4),
      weakUntil(lt(0), is(3)), lt(0) weakUntil is(3), lt(0) W is(3),
      weakUntil(lt(0), is(0)), lt(0) weakUntil is(0), lt(0) W is(0),
      weakUntil(F, is(3)), F weakUntil is(3), F W is(3),
    )
  )
