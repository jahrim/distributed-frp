package it.unibo.distributedfrp.test.utils.logic.temporal

import it.unibo.distributedfrp.test.utils.logic.temporal.LinearTemporalLogic
import it.unibo.distributedfrp.test.utils.logic.Logic
import it.unibo.distributedfrp.test.utils.logic.fol.FirstOrderLogic
import it.unibo.distributedfrp.test.utils.logic.propositional.PropositionalLogic

import scala.annotation.targetName

/**
 * A [[Logic]] that uses propositions with temporal operators to
 * define dynamic properties about a system.
 *
 * @note often abbreviated as `LTL`.
 */
trait LinearTemporalLogic extends FirstOrderLogic:
  /**
   * @tparam S the type of subject of the specified [[Formula Formula]].
   * @return a [[UnaryLogicOperator UnaryLogicOperator]] that maps an input
   *         [[Formula Formula]] into an output [[Formula Formula]] that is satisfied
   *         only if the input [[Formula Formula]] is satisfied in the next state of
   *         the system.
   * @note alias ◯Φ or XΦ (next phi).
   */
  def next[S]: UnaryLogicOperator[S]

  /**
   * @tparam S the type of subject of the specified [[Formula Formula]]s.
   * @return a [[BinaryLogicOperator BinaryLogicOperator]] that maps two input
   *         [[Formula Formula]]s into an output [[Formula Formula]] that is satisfied
   *         only if the first input [[Formula Formula]] is satisfied in every state of
   *         the system before the state when the second input [[Formula Formula]] is
   *         satisfied, eventually reached.
   * @note alias Φ U Ψ (phi until psi).
   */
  def until[S]: BinaryLogicOperator[S]

  /**
   * @tparam S the type of subject of the specified [[Formula Formula]]s.
   * @return a [[BinaryLogicOperator BinaryLogicOperator]] that maps two input
   *         [[Formula Formula]]s into an output [[Formula Formula]] that is satisfied
   *         only if the first input [[Formula Formula]] is satisfied in every state of
   *         the system before the state when the second input [[Formula Formula]] is
   *         satisfied, if ever reached.
   * @note alias Φ W Ψ (phi weakUntil psi).
   */
  def weakUntil[S]: BinaryLogicOperator[S] = (phi, psi) =>
    or(until(phi, psi), always(phi))

  /**
   * @tparam S the type of subject of the specified [[Formula Formula]].
   * @return a [[UnaryLogicOperator UnaryLogicOperator]] that maps an input
   *         [[Formula Formula]] into an output [[Formula Formula]] that is satisfied
   *         only if the input [[Formula Formula]] is satisfied in any state of the system.
   * @note alias ♢Φ or FΦ (sometimes phi).
   */
  def sometimes[S]: UnaryLogicOperator[S] = phi =>
    until(verum, phi)

  /**
   * @tparam S the type of subject of the specified [[Formula Formula]].
   * @return a [[UnaryLogicOperator UnaryLogicOperator]] that maps an input
   *         [[Formula Formula]] into an output [[Formula Formula]] that is satisfied
   *         only if the input [[Formula Formula]] is satisfied in every state of
   *         the system.
   * @note alias ◻Φ or GΦ (always phi).
   */
  def always[S]: UnaryLogicOperator[S] =
    sometimes[S].dual

/** Companion object of [[LinearTemporalLogic]]. */
object LinearTemporalLogic:
  /**
   * A mixin for extending a [[LinearTemporalLogic]] with aliases and infix
   * notation to enhance the api experience and formula readability.
   *
   * @note includes [[FirstOrderLogic.DSL]].
   */
  trait DSL extends FirstOrderLogic.DSL:
    LTL: LinearTemporalLogic =>
    extension[S] (self: Formula[S]) {
      /** Infix notation for [[LinearTemporalLogic.until until]]. */
      @targetName("infixUntil")
      infix def until: UnaryLogicOperator[S]= LTL.until.curried(self)

      /** Infix notation for [[LinearTemporalLogic.weakUntil weakUntil]]. */
      @targetName("infixWeakUntil")
      infix def weakUntil: UnaryLogicOperator[S] = LTL.weakUntil.curried(self)
    }
  end DSL

  /**
   * A mixin for extending a [[LinearTemporalLogic]] with operator
   * shorthands to increase the agility in defining formulas.
   *
   * @note includes [[FirstOrderLogic.Shorthands]].
   */
  trait Shorthands extends FirstOrderLogic.Shorthands:
    LTL: LinearTemporalLogic =>
    /** Operator for [[LinearTemporalLogic.next next]]. */
    @targetName("nextOperator")
    def X[S]: UnaryLogicOperator[S] = LTL.next

    /** Operator for [[LinearTemporalLogic.sometimes sometimes]]. */
    @targetName("sometimesOperator")
    // `F[S]: UnaryLogicOperator[S] (sometimes)` must be disambiguated from `F[S]: Formula[S] (falsum)`
    def F[S](phi: Formula[S]): Formula[S] = LTL.sometimes(phi)

    /** Operator for [[LinearTemporalLogic.always always]]. */
    @targetName("alwaysOperator")
    def G[S]: UnaryLogicOperator[S] = LTL.always

    extension[S] (self: Formula[S]) {
      /** Infix operator for [[LinearTemporalLogic.until until]]. */
      @targetName("infixUntilOperator")
      infix def U: UnaryLogicOperator[S] = LTL.until.curried(self)
      /** Infix operator for [[LinearTemporalLogic.weakUntil weakUntil]]. */
      @targetName("infixWeakUntilOperator")
      infix def W: UnaryLogicOperator[S] = LTL.weakUntil.curried(self)
    }
  end Shorthands

  /**
   * A mixin for extending a [[LinearTemporalLogic]] with a set of
   * the most common inference rules.
   */
  trait RulesOfInference extends PropositionalLogic.RulesOfInference { self: LinearTemporalLogic => }

  /**
   * A mixin for extending a [[LinearTemporalLogic]] with a set of
   * the most common replacement rules.
   */
  trait RulesOfReplacement extends PropositionalLogic.RulesOfReplacement:
    LTL: LinearTemporalLogic =>

    private object InternalDSL extends LinearTemporalLogic with DSL with Shorthands { export LTL.{*, given} }
    import InternalDSL.*

    /** `◯(P && Q) equals (◯P && ◯Q)` */
    def nextDistributivityOverConjunction[S]: BinaryLogicOperator[S] =
      next[S].distributivity(over = and)
    /** `◯(P || Q) equals (◯P || ◯Q)` */
    def nextDistributivityOverDisjunction[S]: BinaryLogicOperator[S] =
      next[S].distributivity(over = or)
    /** `◯(P U Q) equals (◯P U ◯Q)` */
    def nextDistributivityOverUntil[S]: BinaryLogicOperator[S] =
      next[S].distributivity(over = until)
    /** `((P && Q) U R)) equals ((P U R) && (Q U R))` */
    def untilDistributivityOverConjunction[S]: TernaryLogicOperator[S] = (a, b, c) =>
      ((a && b) U c) <--> ((a U c) && (b U c))
    /** `(P U (Q || R)) equals ((P U Q) || (P U R))` */
    def untilDistributivityOverDisjunction[S]: TernaryLogicOperator[S] =
      until[S].distributivity(over = or)
    /** `♢(P || Q) equals (♢P || ♢Q)` */
    def sometimesDistributivityOverDisjunction[S]: BinaryLogicOperator[S] =
      sometimes[S].distributivity(over = or)
    /** `◻(P && Q) equals (◻P && ◻Q)` */
    def alwaysDistributivityOverConjunction[S]: BinaryLogicOperator[S] =
      always[S].distributivity(over = and)

    /** `◯P equals !◯(!P)` */
    def nextSelfDuality[S]: UnaryLogicOperator[S] =
      next[S].duality(wrt = next)
    /** `♢P equals !◻(!P)` */
    def sometimesDualityWithAlways[S]: UnaryLogicOperator[S] =
      sometimes[S].duality(wrt = always)
    /** `◻P equals !♢(!P)` */
    def alwaysDualityWithSometimes[S]: UnaryLogicOperator[S] =
      always[S].duality(wrt = sometimes)

    /** `♢P equals ♢♢P` */
    def sometimesIdempotency[S]: UnaryLogicOperator[S] =
      sometimes[S].idempotency
    /** `◻P equals ◻◻P` */
    def alwaysIdempotency[S]: UnaryLogicOperator[S] =
      always[S].idempotency
    /** `P U Q equals P U (P U Q)` */
    def untilIdempotency[S]: BinaryLogicOperator[S] =
      until[S].idempotency

    /** `P U Q equals (Q || (P && ◯(P U Q)))` */
    def untilRecursion[S]: BinaryLogicOperator[S] = (a, b) =>
      (a U b) <--> (b || (a && X(a U b)))
    /** `P W Q equals (Q || (P && ◯(P W Q)))` */
    def weakUntilRecursion[S]: BinaryLogicOperator[S] = (a, b) =>
      (a W b) <--> (b || (a && X(a W b)))
    /** `♢P equals (P || ◯♢P)` */
    def sometimesRecursion[S]: UnaryLogicOperator[S] = (a) =>
      F(a) <--> (a || X(F(a)))
    /** `◻P equals (P || ◯◻P)` */
    def alwaysRecursion[S]: UnaryLogicOperator[S] = (a) =>
      G(a) <--> (a && X(G(a)))
