package it.unibo.distributedfrp.test.utils.logic.propositional

import it.unibo.distributedfrp.test.utils.logic.{Logic, LogicTest}
import it.unibo.distributedfrp.test.utils.logic.propositional.PropositionalLogic
import it.unibo.distributedfrp.test.utils.logic.propositional.PropositionalLogic.*

/**
 * A [[LogicTest]] extended with testing functionalities for propositional
 * rules of inference and replacement.
 *
 * @tparam L the type of [[PropositionalLogic]] to be tested.
 */
trait PropositionalLogicTest[L <: PropositionalLogic, S] extends LogicTest[L, S]:
  protected val ExFalsoSequiturQuodlibet: FormulaId = symbol("ExFalsoSequiturQuodlibet")
  protected val ModusPonendoPonens: FormulaId = symbol("ModusPonendoPonens")
  protected val ModusTollendoTollens: FormulaId = symbol("ModusTollendoTollens")
  protected val ModusPonendoTollens: FormulaId = symbol("ModusPonendoTollens")
  protected val ModusTollendoPonens: FormulaId = symbol("ModusTollendoPonens")
  protected val Absorption: FormulaId = symbol("Absorption")
  protected val NegationIntroduction: FormulaId = symbol("NegationIntroduction")
  protected val BiconditionalIntroduction: FormulaId = symbol("BiconditionalIntroduction")
  protected val BiconditionalFormerElimination: FormulaId = symbol("BiconditionalFormerElimination")
  protected val BiconditionalLatterElimination: FormulaId = symbol("BiconditionalLatterElimination")
  protected val ConjunctionFormerElimination: FormulaId = symbol("ConjunctionFormerElimination")
  protected val ConjunctionLatterElimination: FormulaId = symbol("ConjunctionLatterElimination")
  protected val DisjunctionFormerIntroduction: FormulaId = symbol("DisjunctionFormerIntroduction")
  protected val DisjunctionLatterIntroduction: FormulaId = symbol("DisjunctionFormerIntroduction")
  protected val DisjunctionElimination: FormulaId = symbol("DisjunctionElimination")
  protected val HyphoteticalSyllogism: FormulaId = symbol("HyphoteticalSyllogism")
  protected val MonotonicityOfEntailment: FormulaId = symbol("MonotonicityOfEntailment")
  protected val IdempotencyOfEntailment: FormulaId = symbol("IdempotencyOfEntailment")
  protected val ConstructiveDilemma: FormulaId = symbol("ConstructiveDilemma")
  protected val DestructiveDilemma: FormulaId = symbol("DestructiveDilemma")

  protected val Identity: FormulaId = symbol("Identity")
  protected val NonContraddiction: FormulaId = symbol("NonContraddiction")
  protected val ExcludedMiddle: FormulaId = symbol("ExcludedMiddle")
  protected val ConjunctionTautology: FormulaId = symbol("ConjunctionTautology")
  protected val DisjunctionTautology: FormulaId = symbol("DisjunctionTautology")
  protected val DoubleNegation: FormulaId = symbol("DoubleNegation")
  protected val ConjunctionDeMorgan: FormulaId = symbol("ConjunctionDeMorgan")
  protected val DisjunctionDeMorgan: FormulaId = symbol("DisjunctionDeMorgan")
  protected val Transposition: FormulaId = symbol("Transposition")
  protected val MaterialConditional: FormulaId = symbol("MaterialConditional")
  protected val Exportation: FormulaId = symbol("Exportation")
  protected val ConjunctionAssociativity: FormulaId = symbol("ConjunctionAssociativity")
  protected val DisjunctionAssociativity: FormulaId = symbol("DisjunctionAssociativity")
  protected val BiconditionalAssociativity: FormulaId = symbol("BiconditionalAssociativity")
  protected val ConjunctionCommutativity: FormulaId = symbol("ConjuctionCommutativity")
  protected val DisjunctionCommutativity: FormulaId = symbol("DisjunctionCommutativity")
  protected val ConditionalCommutativity: FormulaId = symbol("ConditionalCommutativity")
  protected val BiconditionalCommutativity: FormulaId = symbol("BiconditionalCommutativity")
  protected val ConjunctionDistributivityOverConjunction: FormulaId = symbol("ConjunctionDistributivityOverConjunction")
  protected val ConjunctionDistributivityOverDisjunction: FormulaId = symbol("ConjunctionDistributivityOverDisjunction")
  protected val DisjunctionDistributivityOverConjunction: FormulaId = symbol("DisjunctionDistributivityOverConjunction")
  protected val DisjunctionDistributivityOverDisjunction: FormulaId = symbol("DisjunctionDistributivityOverDisjunction")
  protected val DisjunctionDistributivityOverConditional: FormulaId = symbol("DisjunctionDistributivityOverConditional")
  protected val DisjunctionDistributivityOverBiconditional: FormulaId = symbol("DisjunctionDistributivityOverBiconditional")
  protected val ConditionalDistributivityOverConjunction: FormulaId = symbol("ConditionalDistributivityOverConjunction")
  protected val ConditionalDistributivityOverDisjunction: FormulaId = symbol("ConditionalDistributivityOverDisjunction")
  protected val ConditionalDistributivityOverConditional: FormulaId = symbol("ConditionalDistributivityOverConditional")
  protected val ConditionalDistributivityOverBiconditional: FormulaId = symbol("ConditionalDistributivityOverBiconditional")

  private object InternalLogic extends PropositionalLogic with RulesOfInference with RulesOfReplacement:
    export logic.{*, given}
  import InternalLogic.*

  /**
   * @return a [[Seq]] of the [[Formula Formula]]s used as input to test the
   *         validity of inference and replacement rules of within the [[Logic]]
   *         tested by this [[LogicTest]].
   */
  protected def atoms: Seq[Formula[S]]

  /**
   * @return a group of [[Formula Formula]]s for verifying the most common rules
   *         of inference against the [[Logic]] tested by this [[LogicTest]].
   */
  protected def rulesOfInference: Map[FormulaId, Formula[S]] =
    Map[FormulaId, LogicOperator[S]](
      ExFalsoSequiturQuodlibet -> exFalsoSequiturQuodlibet,
      ModusPonendoPonens -> modusPonendoPonens,
      ModusTollendoTollens -> modusTollendoTollens,
      ModusPonendoTollens -> modusPonendoTollens,
      ModusTollendoPonens -> modusTollendoPonens,
      Absorption -> absorbtion,
      NegationIntroduction -> negationIntroduction,
      BiconditionalIntroduction -> biconditionalIntroduction,
      BiconditionalFormerElimination -> biconditionalFormerElimination,
      BiconditionalLatterElimination -> biconditionalLatterElimination,
      ConjunctionFormerElimination -> conjuctionFormerElimination,
      ConjunctionLatterElimination -> conjuctionLatterElimination,
      DisjunctionFormerIntroduction -> disjunctionFormerIntroduction,
      DisjunctionLatterIntroduction -> disjunctionLatterIntroduction,
      DisjunctionElimination -> disjunctionElimination,
      HyphoteticalSyllogism -> hypotheticalSyllogism,
      MonotonicityOfEntailment -> monotonicityOfEntailment,
      IdempotencyOfEntailment -> idempotencyOfEntailment,
      ConstructiveDilemma -> constructiveDilemma,
      DestructiveDilemma -> destructiveDilemma,
    ).map(_ -> forAllAtomCombinations(_))

  /**
   * @return a group of [[Formula Formula]]s for verifying the most common rules
   *         of replacement against the [[Logic]] tested by this [[LogicTest]].
   */
  protected def rulesOfReplacement: Map[FormulaId, Formula[S]] =
    Map[FormulaId, LogicOperator[S]](
      Identity -> identity,
      NonContraddiction -> nonContraddiction,
      ExcludedMiddle -> excludedMiddle,
      ConjunctionTautology -> conjunctionTautology,
      DisjunctionTautology -> disjunctionTautology,
      DoubleNegation -> doubleNegation,
      ConjunctionDeMorgan -> conjunctionDeMorgan,
      DisjunctionDeMorgan -> disjunctionDeMorgan,
      Transposition -> transposition,
      MaterialConditional -> materialConditional,
      Exportation -> exportation,
      ConjunctionAssociativity -> conjunctionAssociativity,
      DisjunctionAssociativity -> disjunctionAssociativity,
      BiconditionalAssociativity -> biconditionalAssociativity,
      ConjunctionCommutativity -> conjunctionCommutativity,
      DisjunctionCommutativity -> disjunctionCommutativity,
      ConditionalCommutativity -> conditionalCommutativity,
      BiconditionalCommutativity -> biconditionalCommutativity,
      ConjunctionDistributivityOverConjunction -> conjunctionDistributivityOverConjunction,
      ConjunctionDistributivityOverDisjunction -> conjunctionDistributivityOverDisjunction,
      DisjunctionDistributivityOverConjunction -> disjunctionDistributivityOverConjunction,
      DisjunctionDistributivityOverDisjunction -> disjunctionDistributivityOverDisjunction,
      DisjunctionDistributivityOverConditional -> disjunctionDistributivityOverConditional,
      DisjunctionDistributivityOverBiconditional -> disjunctionDistributivityOverBiconditional,
      ConditionalDistributivityOverConjunction -> conditionalDistributivityOverConjunction,
      ConditionalDistributivityOverDisjunction -> conditionalDistributivityOverDisjunction,
      ConditionalDistributivityOverConditional -> conditionalDistributivityOverConditional,
      ConditionalDistributivityOverBiconditional -> conditionalDistributivityOverBiconditional,
    ).map(_ -> forAllAtomCombinations(_))

  /**
   * @param op the specified [[LogicOperator]].
   * @return a new [[Formula Formula]] that is verified if the specified [[LogicOperator]]
   *         is satisfied for all possible combinations of [[atoms]].
   */
  // TODO find a way to do type checking on a function
  protected def forAllAtomCombinations(op: LogicOperator[S]) : Formula[S] = op match
    case f: UnaryLogicOperator[S] =>
      atomsCombinations(1).foldLeft(verum)((acc, next) => and(acc, f(next.head)))
    case f: BinaryLogicOperator[S] =>
      atomsCombinations(2).foldLeft(verum)((acc, next) => and(acc, f(next.head, next(1))))
    case f: TernaryLogicOperator[S] =>
      atomsCombinations(3).foldLeft(verum)((acc, next) => and(acc, f(next.head, next(1), next(2))))
    case f: QuaternaryLogicOperator[S] =>
      atomsCombinations(4).foldLeft(verum)((acc, next) => and(acc, f(next.head, next(1), next(2), next(3))))

  /**
   * @param n the specified length.
   * @return all the possible combinations of [[atoms]] of the specified length.
   */
  private def atomsCombinations(n: Int): Seq[Seq[Formula[S]]] =
    if n <= 0 then Seq() else
      Seq.range(0, n-1).foldLeft(atoms.map(Seq(_)))((acc, _) => acc.flatMap(comb => atoms.map(comb :+ _)))
