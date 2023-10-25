package it.unibo.distributedfrp.test.utils.logic.fol

import it.unibo.distributedfrp.test.utils.logic.propositional.PropositionalLogic

import scala.annotation.targetName

/**
 * A [[PropositionalLogic]] extended with predicates that evaluate
 * properties over the states of the system.
 */
trait FirstOrderLogic extends PropositionalLogic:
  /**
   * @param expression the specified logical expression, dependant on
   *                   the current state of the system.
   * @tparam S the type of subject of the expression.
   * @return a [[Formula Formula]] obtained by evaluating the specified
   *         logical expression in the current state of the system.
   * @note entrypoint for user-defined properties.
   */
  def predicate[S](expression: S => Formula[S]): Formula[S]

/** Companion object [[FirstOrderLogic]]. */
object FirstOrderLogic:
  /**
   * A mixin for extending a [[FirstOrderLogic FirstOrderLogic]] with
   * aliases and infix notation to enhance the api experience and
   * formula readability.
   *
   * @note includes [[PropositionalLogic.DSL]].
   */
  trait DSL extends PropositionalLogic.DSL:
    FOL: FirstOrderLogic =>
    /** Alias for [[FirstOrderLogic.predicate predicate]]. */
    def let[S](expression: S => Formula[S]): Formula[S] = FOL.predicate(expression)

  /**
   * A mixin for extending a [[FirstOrderLogic FirstOrderLogic]] with operator
   * shorthands to increase the agility in defining formulas.
   *
   * @note includes [[PropositionalLogic.Shorthands]].
   */
  trait Shorthands extends PropositionalLogic.Shorthands:
    FOL: FirstOrderLogic =>
    /** Shorthand for [[PropositionalLogic.proposition proposition]]. */
    @targetName("predicateShorthand")
    def P[S](expression: S => Formula[S]): Formula[S] = FOL.predicate(expression)

  /**
   * A mixin for extending a [[FirstOrderLogic]] with predicates
   * for comparing subjects.
   */
  trait Comparisons:
    FOL: FirstOrderLogic =>

    /** Alias for [[FOL.predicate `predicate(x => proposition(x == s))`]]. */
    def is[S](s: S): Formula[S] =
      predicate(x => FOL.proposition(x == s))

    /** Alias for [[FOL.predicate `predicate(x => proposition(x != s))`]]. */
    def isnt[S](s: S): Formula[S] =
      FOL.not(is(s))

    /** Alias for [[FOL.predicate `predicate(x => proposition(x < s))`]]. */
    def lt[S](s: S)(using ordering: Ordering[S]): Formula[S] =
      predicate(x => FOL.proposition(ordering.lt(x, s)))

    /** Alias for [[FOL.predicate `predicate(x => proposition(x >= s))`]]. */
    def gteq[S](s: S)(using ordering: Ordering[S]): Formula[S] =
      FOL.not(lt(s))
  end Comparisons
end FirstOrderLogic
