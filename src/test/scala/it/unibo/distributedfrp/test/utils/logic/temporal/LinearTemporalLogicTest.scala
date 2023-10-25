package it.unibo.distributedfrp.test.utils.logic.temporal

import it.unibo.distributedfrp.test.utils.logic.propositional.PropositionalLogicTest
import it.unibo.distributedfrp.test.utils.logic.temporal.LinearTemporalLogic
import it.unibo.distributedfrp.test.utils.logic.temporal.LinearTemporalLogic.*

/**
 * A [[PropositionalLogicTest]] extended with additional inference and
 * replacement rules that should hold for a linear temporal logic.
 *
 * @tparam L the type of [[LinearTemporalLogic]] to be tested.
 */
trait LinearTemporalLogicTest[L <: LinearTemporalLogic, S] extends PropositionalLogicTest[L, S]:
  protected val NextDistributivityOverConjunction: FormulaId = symbol("NextDistributivityOverConjunction")
  protected val NextDistributivityOverDisjunction: FormulaId = symbol("NextDistributivityOverDisjunction")
  protected val NextDistributivityOverUntil: FormulaId = symbol("NextDistributivityOverUntil")
  protected val UntilDistributivityOverConjunction: FormulaId = symbol("UntilDistributivityOverConjunction")
  protected val UntilDistributivityOverDisjunction: FormulaId = symbol("UntilDistributivityOverDisjunction")
  protected val SometimesDistributivityOverDisjunction: FormulaId = symbol("SometimesDistributivityOverDisjunction")
  protected val AlwaysDistributivityOverConjunction: FormulaId = symbol("AlwaysDistributivityOverConjunction")
  protected val NextSelfDuality: FormulaId = symbol("NextSelfDuality")
  protected val SometimesDualityWithAlways: FormulaId = symbol("SometimesDualityWithAlways")
  protected val AlwaysDualityWithSometimes: FormulaId = symbol("AlwaysDualityWithSometimes")
  protected val SometimesIdempotency: FormulaId = symbol("SometimesIdempotency")
  protected val AlwaysIdempotency: FormulaId = symbol("AlwaysIdempotency")
  protected val UntilIdempotency: FormulaId = symbol("UntilIdempotency")
  protected val UntilRecursion: FormulaId = symbol("UntilRecursion")
  protected val WeakUntilRecursion: FormulaId = symbol("WeakUntilRecursion")
  protected val SometimesRecursion: FormulaId = symbol("SometimesRecursion")
  protected val AlwaysRecursion: FormulaId = symbol("AlwaysRecursion")

  private object InternalLogic extends LinearTemporalLogic with RulesOfInference with RulesOfReplacement:
    export logic.{*, given}
  import InternalLogic.*

  override protected def rulesOfReplacement: Map[FormulaId, Formula[S]] = super.rulesOfReplacement ++
    Map[FormulaId, LogicOperator[S]](
      NextDistributivityOverConjunction -> nextDistributivityOverConjunction,
      NextDistributivityOverDisjunction -> nextDistributivityOverDisjunction,
      NextDistributivityOverUntil -> nextDistributivityOverUntil,
      UntilDistributivityOverConjunction -> untilDistributivityOverConjunction,
      UntilDistributivityOverDisjunction -> untilDistributivityOverDisjunction,
      SometimesDistributivityOverDisjunction -> sometimesDistributivityOverDisjunction,
      AlwaysDistributivityOverConjunction -> alwaysDistributivityOverConjunction,
      NextSelfDuality -> nextSelfDuality,
      SometimesDualityWithAlways -> sometimesDualityWithAlways,
      AlwaysDualityWithSometimes -> alwaysDualityWithSometimes,
      SometimesIdempotency -> sometimesIdempotency,
      AlwaysIdempotency -> alwaysIdempotency,
      UntilIdempotency -> untilIdempotency,
      UntilRecursion -> untilRecursion,
      WeakUntilRecursion -> weakUntilRecursion,
      SometimesRecursion -> sometimesRecursion,
      AlwaysRecursion -> alwaysRecursion,
    ).map(_ -> forAllAtomCombinations(_))
