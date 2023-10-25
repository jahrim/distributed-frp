package it.unibo.distributedfrp.test.utils.logic

/**
 * A formal language to define formulas that describe system properties.
 */
trait Logic:
  /**
   * A formula in this [[Logic]], describing a property of the system.
   *
   * @tparam S the type of the subject of the [[Formula Formula]].
   */
  type Formula[S]
  /**
   * An evaluation for a [[Formula Formula]] in this [[Logic]],
   * describing if the [[Formula Formula]] is satisfied or not.
   */
  type Evaluation
  /**
   * An evaluator for the [[Formula Formula]]s in this [[Logic]],
   * producing [[Evaluation Evaluation]]s for those [[Formula Formula]]s.
   *
   * @tparam S the type of the subject of the [[Formula Formula]]s.
   */
  type Evaluator[S] = Formula[S] => Evaluation

  /** Convert a [[Boolean]] into an [[Evaluation]]. */
  given booleanToEvaluation: Conversion[Boolean, Evaluation]

  extension (self: Evaluation){
    /**
     * @return true if the [[Formula Formula]] described by this
     *         [[Evaluation Evaluation]] is satisfied; false otherwise.
     */
    def isSatisfied: Boolean
    /**
     * @return true if the [[Formula Formula]] described by this
     *         [[Evaluation Evaluation]] is falsified; false otherwise.
     */
    def isFalsified: Boolean
  }

  /**
   * An operator that combines logical [[Formula Formula]]s into other
   * logical [[Formula Formula]]s.
   */
  type LogicOperator[S] =
    UnaryLogicOperator[S] |
    BinaryLogicOperator[S] |
    TernaryLogicOperator[S] |
    QuaternaryLogicOperator[S]
  /**
   * A logic operator that maps a [[Formula Formula]] into another
   * [[Formula Formula]].
   */
  type UnaryLogicOperator[S] = Formula[S] => Formula[S]
  /**
   * A logic operator that combines two [[Formula Formula]]s into another
   * [[Formula Formula]].
   */
  type BinaryLogicOperator[S] = (Formula[S], Formula[S]) => Formula[S]
  /**
   * A logic operator that combines three [[Formula Formula]]s into another
   * [[Formula Formula]].
   */
  type TernaryLogicOperator[S] = (Formula[S], Formula[S], Formula[S]) => Formula[S]
  /**
   * A logic operator that combines four [[Formula Formula]]s into another
   * [[Formula Formula]].
   */
  type QuaternaryLogicOperator[S] = (Formula[S], Formula[S], Formula[S], Formula[S]) => Formula[S]
