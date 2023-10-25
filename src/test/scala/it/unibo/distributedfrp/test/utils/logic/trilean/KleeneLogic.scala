package it.unibo.distributedfrp.test.utils.logic.trilean

import it.unibo.distributedfrp.test.utils.logic.trilean.ThreeValueLogic
import it.unibo.distributedfrp.test.utils.logic.Logic

/**
 * A mixin for a Kleene three-valued [[Logic]] (K3) that allows
 * to define the validity of a formula in terms of [[Trilean]]s.
 *
 * In K3, tautologies do not hold in general, mainly because any
 * formula that can be reduced to the form `U <--> U` is not satisfied,
 * but it yields unknown.
 *
 * Tautologies may still hold in certain cases, for example if none of the
 * involved logical expressions evaluate to unknown.
 */
trait KleeneLogic extends ThreeValueLogic:
  extension (self: Trilean){
    override def isSatisfied: Boolean = self == True
    override def isFalsified: Boolean = self == False
    override def isUnknown: Boolean = self == Unknown
  }
