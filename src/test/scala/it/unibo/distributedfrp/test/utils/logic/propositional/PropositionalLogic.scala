package it.unibo.distributedfrp.test.utils.logic.propositional

import it.unibo.distributedfrp.test.utils.logic.propositional.PropositionalLogic
import it.unibo.distributedfrp.test.utils.logic.temporal.LinearTemporalLogic
import it.unibo.distributedfrp.test.utils.logic.Logic

import scala.annotation.targetName

/**
 * A [[Logic]] that uses propositions to define static properties
 * about a system.
 */
trait PropositionalLogic extends Logic:
  /**
   * @tparam S the type of subject of the tautology.
   * @return a tautology on the specified subject
   *         (i.e. a property that cannot be falsified).
   * @note alias ⊤ (verum, tee or down-tack).
   */
  def verum[S]: Formula[S] =
    val p: Formula[S] = proposition[S](true)
    or(p, not(p))

  /**
   * @tparam S the type of subject of the contradiction.
   * @return a contradiction on the specified subject
   *         (i.e. a property that cannot be satisfied).
   * @note alias ⊥ (falsum or up-tack).
   */
  def falsum[S]: Formula[S] =
    not(verum)

  /**
   * @param truth the specified truth value.
   * @tparam S the type of subject of the proposition.
   * @return a proposition on the specified subject, whose validity
   *         is determined by the specified truth value.
   * @note entrypoint for user-defined properties.
   */
  def proposition[S](truth: Evaluation): Formula[S]

  /**
   * @tparam S the type of subject of the specified [[Formula Formula]].
   * @return a [[UnaryLogicOperator UnaryLogicOperator]] that maps an input
   *         [[Formula Formula]] into an output [[Formula Formula]] that is satisfied
   *         only if the input [[Formula Formula]] is not satisfied.
   * @note alias ¬Φ (not phi).
   */
  def not[S]: UnaryLogicOperator[S]

  /**
   * @tparam S the type of subject of the specified [[Formula Formula]]s.
   * @return a [[BinaryLogicOperator BinaryLogicOperator]] that maps two input
   *         [[Formula Formula]]s into an output [[Formula Formula]] that is satisfied
   *         only if both of the input [[Formula Formula]]s are satisfied.
   * @note alias Φ ∧ Ψ (phi and psi).
   */
  def and[S]: BinaryLogicOperator[S]

  /**
   * @tparam S the type of subject of the specified [[Formula Formula]]s.
   * @return a [[BinaryLogicOperator BinaryLogicOperator]] that maps two input
   *         [[Formula Formula]]s into an output [[Formula Formula]] that is satisfied
   *         only if any of the input [[Formula Formula]]s are satisfied.
   * @note alias Φ ∨ Ψ (phi or psi).
   */
  def or[S]: BinaryLogicOperator[S] =
    and[S].dual

  /**
   * @tparam S the type of subject of the specified [[Formula Formula]]s.
   * @return a [[BinaryLogicOperator BinaryLogicOperator]] that maps two input
   *         [[Formula Formula]]s into an output [[Formula Formula]] that is satisfied
   *         only if the first input [[Formula Formula]]s implies the second one.
   * @note alias Φ → Ψ (phi implies psi).
   */
  def conditional[S]: BinaryLogicOperator[S] = (phi, psi) =>
    or(not(phi), psi)

  /**
   * @tparam S the type of subject of the specified [[Formula Formula]]s.
   * @return a [[BinaryLogicOperator BinaryLogicOperator]] that maps two input
   *         [[Formula Formula]]s into an output [[Formula Formula]] that is satisfied
   *         only if the input [[Formula Formula]]s imply each other (i.e. only if they
   *         are equivalent).
   * @note alias Φ ↔ Ψ (phi equals psi)
   */
  def biconditional[S]: BinaryLogicOperator[S] = (phi, psi) =>
    and(conditional(phi, psi), conditional(psi, phi))

  extension[S] (self: UnaryLogicOperator[S]) {
    /** @return `¬A(Φ)`, where `A(Φ)` is this [[UnaryOperator UnaryOperator]]. */
    protected def negation: UnaryLogicOperator[S] = phi => not(self(phi))
    /** @return `A(¬Φ)`, where `A(Φ)` is this [[UnaryOperator UnaryOperator]]. */
    protected def simmetry: UnaryLogicOperator[S] = phi => self(not(phi))
    /** @return `¬A(¬Φ)`, where `A(Φ)` is this [[UnaryOperator UnaryOperator]]. */
    protected def dual: UnaryLogicOperator[S] = self.simmetry.negation
  }

  extension[S] (self: BinaryLogicOperator[S]) {
    /** @return `¬A(Φ,Ψ)`, where `A(Φ,Ψ)` is this [[BinaryOperator BinaryOperator]]. */
    protected def negation: BinaryLogicOperator[S] = (phi, psi) => not(self(phi, psi))
    /** @return `A(¬Φ,¬Ψ)`, where `A(Φ,Ψ)` is this [[BinaryOperator BinaryOperator]]. */
    protected def simmetry: BinaryLogicOperator[S] = (phi, psi) => self(not(phi), not(psi))
    /** @return `¬A(¬Φ,¬Ψ)`, where `A(Φ,Ψ)` is this [[BinaryOperator BinaryOperator]]. */
    protected def dual: BinaryLogicOperator[S] = self.simmetry.negation
  }

/** Companion object of [[PropositionalLogic]]. */
object PropositionalLogic:
  /**
   * A mixin for extending a [[PropositionalLogic]] with aliases and infix
   * notation to enhance the api experience and formula readability.
   */
  trait DSL:
    PL: PropositionalLogic =>

    extension[S] (self: Formula[S]) {
      /** Infix notation for [[PropositionalLogic.and and]]. */
      @targetName("infixAnd")
      infix def and: UnaryLogicOperator[S] = PL.and.curried(self)
      /** Infix notation for [[PropositionalLogic.or or]]. */
      @targetName("infixOr")
      infix def or: UnaryLogicOperator[S] = PL.or.curried(self)
      /** Infix notation for [[PropositionalLogic.conditional conditional]]. */
      @targetName("infixConditional")
      infix def implies: UnaryLogicOperator[S] = PL.conditional.curried(self)
      /** Infix notation for [[PropositionalLogic.biconditional biconditional]]. */
      @targetName("infixBiconditional")
      infix def conforms: UnaryLogicOperator[S] = PL.biconditional.curried(self)
    }
  end DSL

  /**
   * A mixin for extending a [[LinearTemporalLogic]] with operator
   * shorthands to increase the agility in defining formulas.
   */
  trait Shorthands:
    PL: PropositionalLogic =>

    /** Shorthand for [[PropositionalLogic.verum verum (True)]].  */
    @targetName("verumShorthand")
    def T[S]: Formula[S] = PL.verum

    /** Shorthand for [[PropositionalLogic.falsum falsum (False)]]. */
    @targetName("falsumShorthand")
    def F[S]: Formula[S] = PL.falsum

    /** Shorthand for [[PropositionalLogic.proposition proposition]]. */
    @targetName("propositionShorthand")
    def ?[S](value: Evaluation): Formula[S] = PL.proposition(value)

    extension[S] (self: Formula[S]) {
      /** Infix operator for [[PropositionalLogic.not not]]. */
      @targetName("infixNotOperator")
      infix def unary_! : Formula[S] = PL.not(self)
      /** Infix operator for [[PropositionalLogic.and and]]. */
      @targetName("infixAndOperator")
      infix def && : UnaryLogicOperator[S] = PL.and.curried(self)
      /** Infix operator for [[PropositionalLogic.or or]]. */
      @targetName("infixOrOperator")
      infix def || : UnaryLogicOperator[S] = PL.or.curried(self)
      /** Infix operator for [[PropositionalLogic.conditional conditional]]. */
      @targetName("infixConditionalOperator")
      infix def ---> : UnaryLogicOperator[S] = PL.conditional.curried(self)
      /** Infix operator for [[PropositionalLogic.biconditional biconditional]]. */
      @targetName("infixBiconditionalOperator")
      infix def <--> : UnaryLogicOperator[S] = PL.biconditional.curried(self)
    }
  end Shorthands

  /**
   * A mixin for extending a [[PropositionalLogic]] with a set of
   * the most common inference rules.
   */
  trait RulesOfInference:
    PL: PropositionalLogic =>

    private object InternalDSL extends PropositionalLogic with DSL with Shorthands { export PL.{*, given} }
    import InternalDSL.*

    /** `⊥ implies P` */
    def exFalsoSequiturQuodlibet[S]: UnaryLogicOperator[S] = a =>
      F ---> a
    /** `((P ---> Q) && P) implies Q` */
    def modusPonendoPonens[S]: BinaryLogicOperator[S] = (a, b) =>
      ((a ---> b) && a) ---> b
    /** `((P ---> Q) && !Q) implies !P` */
    def modusTollendoTollens[S]: BinaryLogicOperator[S] = (a, b) =>
      ((a ---> b) && !b) ---> !a
    /** `(!(P && Q) && P) implies !Q` */
    def modusPonendoTollens[S]: BinaryLogicOperator[S] = (a, b) =>
      (!(a && b) && a) ---> !b
    /** `((P || Q) && !P) implies Q` */
    def modusTollendoPonens[S]: BinaryLogicOperator[S] = (a, b) =>
      ((a || b) && !a) ---> b
    /** `(P ---> Q) implies (P ---> (P && Q))` */
    def absorbtion[S]: BinaryLogicOperator[S] = (a, b) =>
      (a ---> b) ---> (a ---> (a && b))
    /** `((P ---> Q) && (P ---> !Q)) implies !P` */
    def negationIntroduction[S]: BinaryLogicOperator[S] = (a, b) =>
      ((a ---> b) && (a ---> !b)) ---> !a
    /** `((P ---> Q) && (Q ---> P)) implies (P <--> Q)` */
    def biconditionalIntroduction[S]: BinaryLogicOperator[S] = (a, b) =>
      ((a ---> b) && (b ---> a)) ---> (a <--> b)
    /** `(P <--> Q) implies (Q ---> P)` */
    def biconditionalFormerElimination[S]: BinaryLogicOperator[S] = (a, b) =>
      (a <--> b) ---> (b ---> a)
    /** `(P <--> Q) implies (P ---> Q)` */
    def biconditionalLatterElimination[S]: BinaryLogicOperator[S] = (a, b) =>
      (a <--> b) ---> (a ---> b)
    /** `(P && Q) implies Q` */
    def conjuctionFormerElimination[S]: BinaryLogicOperator[S] = (a, b) =>
      (a && b) ---> b
    /** `(P && Q) implies P` */
    def conjuctionLatterElimination[S]: BinaryLogicOperator[S] = (a, b) =>
      (a && b) ---> a
    /** `P implies (P || Q)` */
    def disjunctionFormerIntroduction[S]: BinaryLogicOperator[S] = (a, b) =>
      a ---> (a || b)
    /** `Q implies (P || Q)` */
    def disjunctionLatterIntroduction[S]: BinaryLogicOperator[S] = (a, b) =>
      b ---> (a || b)
    /** `((P ---> R) && (Q ---> R) && (P || Q)) implies R` */
    def disjunctionElimination[S]: TernaryLogicOperator[S] = (a, b, c) =>
      ((a ---> c) && (b ---> c) && (a || b)) ---> c
    /** `((P ---> Q) && (Q ---> R)) implies (P ---> R)` */
    def hypotheticalSyllogism[S]: TernaryLogicOperator[S] = (a, b, c) =>
      ((a ---> b) && (b ---> c)) ---> (a ---> c)
    /** `P ---> R implies (P && Q) ---> R` */
    def monotonicityOfEntailment[S]: TernaryLogicOperator[S] = (a, b, c) =>
      (a ---> b) ---> ((a && c) ---> b)
    /** `(P && Q && Q) ---> R implies (P && Q) ---> Q` */
    def idempotencyOfEntailment[S]: TernaryLogicOperator[S] = (a, b, c) =>
      ((a && c && c) ---> b) ---> ((a && c) ---> b)
    /** `((P ---> R) && (Q ---> S) && (P || Q)) implies (R || S)` */
    def constructiveDilemma[S]: QuaternaryLogicOperator[S] = (a, b, c, d) =>
      ((a ---> c) && (b ---> d) && (a || b)) ---> (c || d)
    /** `((P ---> R) && (Q ---> S) && (!R || !S)) implies (!P || !Q)` */
    def destructiveDilemma[S]: QuaternaryLogicOperator[S] = (a, b, c, d) =>
      ((a ---> c) && (b ---> d) && (!c || !d)) ---> (!a || !b)
  end RulesOfInference

  /**
   * A mixin for extending a [[PropositionalLogic]] with a set of
   * the most common replacement rules.
   */
  trait RulesOfReplacement:
    PL: PropositionalLogic =>

    private object InternalDSL extends PropositionalLogic with DSL with Shorthands { export PL.{*, given} }
    import InternalDSL.*

    /** `P equals P` */
    def identity[S]: UnaryLogicOperator[S] = a =>
      a <--> a
    /** `!(P && !P) equals T` */
    def nonContraddiction[S]: UnaryLogicOperator[S] = a =>
      !(a && !a) <--> T
    /** `(P || !P) equals T` */
    def excludedMiddle[S]: UnaryLogicOperator[S] = a =>
      (a || !a) <--> T
    /** `(P && P) equals P` */
    def conjunctionTautology[S]: UnaryLogicOperator[S] = a =>
      (a && a) <--> a
    /** `(P || P) equals P` */
    def disjunctionTautology[S]: UnaryLogicOperator[S] = a =>
      (a || a) <--> a
    /** `P equals !!P` */
    //noinspection DoubleNegationScala
    def doubleNegation[S]: UnaryLogicOperator[S] = a =>
      a <--> !(!a)
    /** `(P && Q) equals !(!P || !Q)` */
    def conjunctionDeMorgan[S]: BinaryLogicOperator[S] =
      and[S].duality(wrt = or)
    /** `(P || Q) equals !(!P && !Q)` */
    def disjunctionDeMorgan[S]: BinaryLogicOperator[S] =
      or[S].duality(wrt = and)
    /** `(P ---> Q) equals (!Q ---> !P)` */
    def transposition[S]: BinaryLogicOperator[S] = (a, b) =>
      (a ---> b) <--> (!b ---> !a)
    /** `(P ---> Q) equals (!P || Q)` */
    def materialConditional[S]: BinaryLogicOperator[S] = (a, b) =>
      (a ---> b) <--> (!a || b)
    /** `((P && Q) ---> R) equals (P ---> (Q ---> R))` */
    def exportation[S]: TernaryLogicOperator[S] = (a, b, c) =>
      ((a && b) ---> c) <--> (a ---> (b ---> c))

    /** `((P && Q) && R) equals (P && (Q && R))` */
    def conjunctionAssociativity[S]: TernaryLogicOperator[S] =
      and[S].associativity
    /** `((P || Q) || R) equals (P || (Q || R))` */
    def disjunctionAssociativity[S]: TernaryLogicOperator[S] =
      or[S].associativity
    /** `((P <--> Q) <--> R) equals (P <--> (Q <--> R))` */
    def biconditionalAssociativity[S]: TernaryLogicOperator[S] =
      biconditional[S].associativity

    /** `(P && Q) equals (Q && P)` */
    def conjunctionCommutativity[S]: BinaryLogicOperator[S] =
      and[S].commutativity
    /** `(P || Q) equals (Q || P)` */
    def disjunctionCommutativity[S]: BinaryLogicOperator[S] =
      or[S].commutativity
    /** `(P ---> (Q ---> R)) equals (Q ---> (P ---> R))` */
    def conditionalCommutativity[S]: TernaryLogicOperator[S] = (a, b, c) =>
      (a ---> (b ---> c)) <--> (b ---> (a ---> c))
    /** `(P <--> Q) equals (Q <--> P)` */
    def biconditionalCommutativity[S]: BinaryLogicOperator[S] =
      biconditional[S].commutativity

    /** `(P && (Q && R)) equals ((P && Q) && (P && R))` */
    def conjunctionDistributivityOverConjunction[S]: TernaryLogicOperator[S] =
      and[S].distributivity(over = and)
    /** `(P && (Q || R)) equals ((P && Q) || (P && R))` */
    def conjunctionDistributivityOverDisjunction[S]: TernaryLogicOperator[S] =
      and[S].distributivity(over = or)
    /** `(P || (Q && R)) equals ((P || Q) && (P || R))` */
    def disjunctionDistributivityOverConjunction[S]: TernaryLogicOperator[S] =
      or[S].distributivity(over = and)
    /** `(P || (Q || R)) equals ((P || Q) || (P || R))` */
    def disjunctionDistributivityOverDisjunction[S]: TernaryLogicOperator[S] =
      or[S].distributivity(over = or)
    /** `(P || (Q ---> R)) equals ((P || Q) ---> (P || R))` */
    def disjunctionDistributivityOverConditional[S]: TernaryLogicOperator[S] =
      or[S].distributivity(over = conditional)
    /** `(P || (Q <--> R)) equals ((P || Q) <--> (P || R))` */
    def disjunctionDistributivityOverBiconditional[S]: TernaryLogicOperator[S] =
      or[S].distributivity(over = biconditional)
    /** `(P ---> (Q && R)) equals ((P ---> Q) && (P ---> R))` */
    def conditionalDistributivityOverConjunction[S]: TernaryLogicOperator[S] =
      conditional[S].distributivity(over = and)
    /** `(P ---> (Q || R)) equals ((P ---> Q) || (P ---> R))` */
    def conditionalDistributivityOverDisjunction[S]: TernaryLogicOperator[S] =
      conditional[S].distributivity(over = or)
    /** `(P ---> (Q ---> R)) equals ((P ---> Q) ---> (P ---> R))` */
    def conditionalDistributivityOverConditional[S]: TernaryLogicOperator[S] =
      conditional[S].distributivity(over = conditional)
    /** `(P ---> (Q <--> R)) equals ((P ---> Q) <--> (P ---> R))` */
    def conditionalDistributivityOverBiconditional[S]: TernaryLogicOperator[S] =
      conditional[S].distributivity(over = biconditional)

    extension[S] (self: UnaryLogicOperator[S]) {
      /** `op(P) equals op(op(P))` */
      protected def idempotency: UnaryLogicOperator[S] =
        (a) => self(a) <--> self(self(a))
      /** `op1(P) equals !op2(!P)` */
      protected def duality(wrt: UnaryLogicOperator[S]): UnaryLogicOperator[S] =
        (a) => self(a) <--> wrt.dual(a)
      /** `op1(P op2 Q) equals (op1(P) op2 op1(Q))` */
      protected def distributivity(over: BinaryLogicOperator[S]): BinaryLogicOperator[S] =
        (a, b) => self(over(a, b)) <--> over(self(a), self(b))
    }
    extension [S](self: BinaryLogicOperator[S]){
      /** `op(P, Q) equals op(P, op(P, Q))` */
      protected def idempotency: BinaryLogicOperator[S] =
        (a, b) => self(a, b) <--> self(a, self(a, b))
      /** `op1(P, Q) equals !op2(!P, !Q)` */
      protected def duality(wrt: BinaryLogicOperator[S]): BinaryLogicOperator[S] =
        (a, b) => self(a, b) <--> wrt.dual(a, b)
      /** `(P op Q) equals (Q op P)` */
      protected def commutativity: BinaryLogicOperator[S] =
        (a, b) => self(a, b) <--> self(b, a)
      /** `((P op Q) op R) equals (P op (Q op R))` */
      protected def associativity: TernaryLogicOperator[S] =
        (a, b, c) => self(self(a, b), c) <--> self(a, self(b, c))
      /** `((P op1 (Q op2 R)) equals ((P op1 Q) op2 (P op1 R))` */
      protected def distributivity(over: BinaryLogicOperator[S]): TernaryLogicOperator[S] =
        (a, b, c) => self(a, over(b, c)) <--> over(self(a, b), self(a, c))
    }
  end RulesOfReplacement
