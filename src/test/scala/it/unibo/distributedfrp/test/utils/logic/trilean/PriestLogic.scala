package it.unibo.distributedfrp.test.utils.logic.trilean

import it.unibo.distributedfrp.test.utils.logic.trilean.ThreeValueLogic
import it.unibo.distributedfrp.test.utils.logic.Logic

/**
 * A mixin for the Priest [[Logic]] of Paradox (LP) that allows
 * to define the validity of a formula in terms of [[Trilean]]s.
 *
 * In LP, tautologies do hold in general, mainly because a formula
 * is considered to be satisfied even when it is unknown, so that
 * any formula that can be reduced to `U <--> U` (which yields unknown)
 * is considered to be satisfied.
 */
trait PriestLogic extends ThreeValueLogic:
  extension (self: Trilean){
    /** @return true if the formula is satisfied or unknown; false otherwise. */
    override def isSatisfied: Boolean = self == True || isUnknown
    override def isFalsified: Boolean = self == False
    override def isUnknown: Boolean = self == Unknown
  }
