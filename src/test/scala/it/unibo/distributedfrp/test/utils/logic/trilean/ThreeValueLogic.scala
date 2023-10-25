package it.unibo.distributedfrp.test.utils.logic.trilean

import it.unibo.distributedfrp.test.utils.logic.Logic

import scala.annotation.targetName

/**
 * A mixin for a three-valued [[Logic]] that allows to define the validity
 * of a formula in terms of [[Trilean]]s.
 *
 * A [[Trilean]] is a truth value for a formula in a [[ThreeValueLogic]].
 * It can be one of the following:
 *  - [[True]]: the formula is satisfied.
 *  - [[False]]: the formula is falsified.
 *  - [[Unknown]]: the formula cannot be satisfied or
 *    falsified, or it is both satisfied and falsified
 *    (depending on the interpretation).
 */
trait ThreeValueLogic extends Logic:
  override type Evaluation = Trilean
  override given booleanToEvaluation: Conversion[Boolean, Evaluation] =
    b => if b then True else False

  /** @see [[ThreeValueLogic]] for more information. */
  enum Trilean { case True, False, Unknown }
  export Trilean.*

  /**
   * @tparam S the type of subject of the unknown statement.
   * @return an unknown statement on the specified subject (i.e. a
   *         property that cannot be satisfied or falsified, or
   *         it is both satisfied and falsified, depending on the
   *         interpretation).
   * @note alias U (unknown).
   */
  def ignotum[S]: Formula[S]

  extension (self: Evaluation){
    /**
     * @return true if the [[Formula Formula]] described by this
     *         [[Evaluation Evaluation]] is neither satisfied or
     *         falsified (or both satisfied and falsified, depending
     *         on the interpretation); false otherwise.
     */
    def isUnknown: Boolean

    @targetName("infixNotOperator")
    protected infix def unary_! : Trilean = self match
      case True => False
      case False => True
      case Unknown => Unknown
    @targetName("infixAndOperator")
    protected infix def &&(other: Trilean): Trilean = (self, other) match
      case (True, True) => True
      case (False, _) | (_, False) => False
      case _ => Unknown
    @targetName("infixOrOperator")
    protected infix def ||(other: Trilean): Trilean = !(!self && !other)
  }

/** Companion object of [[ThreeValueLogic]]. */
object ThreeValueLogic:
  /**
   * A mixin for extending a [[ThreeValueLogic]] with operator
   * shorthands to increase the agility in defining formulas.
   */
  trait Shorthands:
    TVL: ThreeValueLogic =>
    /** Shorthand for [[ThreeValueLogic.ignotum ignotum (Unknown)]]. */
    def U[S]: Formula[S] = TVL.ignotum
