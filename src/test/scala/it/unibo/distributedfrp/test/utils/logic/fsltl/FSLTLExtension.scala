package it.unibo.distributedfrp.test.utils.logic.fsltl

import it.unibo.distributedfrp.frp.FiniteStreamExtension.{FiniteEvent, FiniteStream}
import it.unibo.distributedfrp.frp.FiniteStreamExtension.FiniteEvent.*
import it.unibo.distributedfrp.test.utils.logic.temporal.LinearTemporalLogic
import it.unibo.distributedfrp.test.utils.logic.trilean.{KleeneLogic, PriestLogic, ThreeValueLogic}

import java.util.concurrent.CancellationException
import scala.concurrent.{ExecutionContext, Future, Promise}

/** An extension for applying [[LinearTemporalLogic]] to [[FiniteStream FiniteStream]]s. */
object FSLTLExtension:
  /** Short name for [[FiniteStreamLinearTemporalLogic FiniteStreamLinearTemporalLogic]]. */
  type FSLTL = FiniteStreamLinearTemporalLogic
  /**
   * Short name for the companion object of
   * [[FiniteStreamLinearTemporalLogic FiniteStreamLinearTemporalLogic]].
   */
  def FSLTL: FiniteStreamLinearTemporalLogic.type = FiniteStreamLinearTemporalLogic

  extension [A, L <: FSLTL](self: FiniteStream[A])(using LTL: L){
    /**
     * Check the specified [[LTL.Formula FSLTL.Formula]] against this
     * [[FiniteStream FiniteStream]].
     *
     * @param phi the specified [[LTL.Formula FSLTL.Formula]].
     * @return an [[LTL.AsyncEvaluation FSLTL.AsyncEvaluation]] of the
     *         specified [[LTL.Formula FSLTL.Formula]].
     * @see [[FiniteStreamLinearTemporalLogic FSLTL]] for more information.
     */
    def check(phi: LTL.Formula[A]): LTL.AsyncEvaluation = phi(using self)
  }

  /**
   * A [[ThreeValueLogic]] and [[LinearTemporalLogic]] for defining properties about a
   * [[FiniteStream FiniteStream]].
   *
   * In particular, a [[FiniteStream FiniteStream]] can be considered a system whose states
   * are the events that the stream will fire.
   * In this perspective, the initial state of a [[FiniteStream FiniteStream]] is the first
   * event that the stream will fire, while the terminal state of the [[FiniteStream FiniteStream]]
   * is the first fired [[EOS EOS (End Of Stream)]] event.
   *
   * Since [[FiniteStream FiniteStream]]s are only '''potentially''' finite, by purposely creating
   * [[FiniteStream FiniteStream]]s that fires no [[EOS EOS]] events, it is also possible to evaluate
   * properties on infinite [[Stream Stream]]s, although it is not advised as the evaluation may take
   * an infinite amount of time depending on the property.
   *
   * The property evaluations are executed incrementally on the specified [[ExecutionContext]] as the
   * [[FiniteStream FiniteStream]] fires new events (i.e. the system changes state). The computation
   * stops as soon as possible after the evaluation has been determined.
   *
   * An evaluation can yield one of the following truth values, complying to the [[ThreeValueLogic]]:
   *  - [[True]]: the [[Formula Formula]] is satisfied;
   *  - [[False]]: the [[Formula Formula]] is not satisfied;
   *  - [[Unknown]]: the [[Formula Formula]] could not be determined. In particular, any
   *    [[Formula Formula]] checked against an empty system yields `Unknown` (even tautologies and
   *    contradictions).
   *
   * @param context the [[ExecutionContext]] where the property evaluations will be run.
   * @note you can refer to this class and its companion object with the alias [[FSLTL FSLTL]].
   * @note to create an instance of this class, you can extend it with either [[KleeneLogic]] or
   *       [[PriestLogic]], depending on how you want to treat [[Unknown]] truth values.
   * @see [[FSLTL.DSL]] if you are interested in an equivalent but '''more declarative''' api, or
   *      [[FSLTL.Shorthands]] if you are interested in a '''more succinct''' api.
   */
  abstract class FiniteStreamLinearTemporalLogic(using context: ExecutionContext)
    extends LinearTemporalLogic
    with ThreeValueLogic:

    override type Formula[S] = FiniteStream[S] ?=> AsyncEvaluation

    /** An asynchronous delayed [[Evaluation Evaluation]]. */
    type AsyncEvaluation = Future[Evaluation]

    override def ignotum[S]: Formula[S] =
      Future.successful(Unknown)
    override def predicate[S](expression: S => Formula[S]): Formula[S] =
      evaluate(evaluation =>
        currentState[S] {
          case Event(payload) => evaluation.completeWith(expression(payload))
          case EOS => evaluation.success(Unknown)
        }
      )
    override def proposition[S](truth: Evaluation): Formula[S] =
      predicate[S](_ => evaluate(evaluation => evaluation.success(truth)))
    override def not[S]: UnaryLogicOperator[S] = phi =>
      phi.map(!_)
    override def and[S]: BinaryLogicOperator[S] = (phi, psi) =>
      val (phiEvaluation, psiEvaluation): (AsyncEvaluation, AsyncEvaluation) = (phi, psi)
      phiEvaluation.flatMap(phiTruth => psiEvaluation.map(psiTruth => phiTruth && psiTruth))
    override def next[S]: UnaryLogicOperator[S] = phi =>
      predicate[S](_ => evaluate(evaluation => nz.sodium.Transaction.post(() => evaluation.completeWith(phi))))
    override def until[S]: BinaryLogicOperator[S] = (phi, psi) =>
      evaluate(initialEvaluation => initialEvaluation.completeWith(untilStep(initialEvaluation.future)(phi, psi)))

    /**
     * Define the [[until]] operator recursively and incrementally, as the states of this
     * [[FiniteStream FiniteStream]] are generated.
     *
     * === Algorithm ===
     * This method implements the following recursive equivalence:
     * <p>Φ U Ψ ≡ Ψ ∨ (Φ ∧ ◯(Φ U Ψ))</p>
     * Let's rewrite the formula as follows:
     * <p>Φ U<sub>s</sub> Ψ ≡ Ψ<sub>s</sub> ∨ (Φ<sub>s</sub> ∧ (Φ U<sub>s+1</sub> Ψ)) with s: Int ∈ [0; +∞]</p>
     * where (Φ U<sub>s</sub> Ψ), (Ψ<sub>s</sub>) and (Φ<sub>s</sub>) are the evaluations of (Φ U Ψ), (Ψ) and (Φ)
     * respectively in the state s.
     *
     * To be clear, this method evaluates Φ U Ψ in the current state (i.e. the next event of the stream),
     * that is Φ U<sub>s</sub> Ψ.
     *
     * In detail, if called at initial state '''s0''', the algorithm works as follows:
     *  1. Name '''s''' the '''current state''' starting from '''s0''', then do the following:
     *     - If Φ U<sub>s0</sub> Ψ has '''yet to be determined''':
     *       1. Configure the concurrent evaluation of Φ<sub>s</sub> and Ψ<sub>s</sub> for when '''s'''
     *          will be fired.
     *       1. When '''s''' is fired, do the following:
     *          - If '''s''' is not '''terminal''':
     *            1. Configure the concurrent evaluation of Φ U<sub>s+1</sub> Ψ, restarting from step 1.
     *            1. Complete the evaluation of Φ U<sub>s</sub> Ψ as follows:
     *               1. If Ψ<sub>s</sub> holds, then Φ U<sub>s</sub> Ψ holds.
     *               1. Else, if Φ<sub>s</sub> does not hold, then Φ U<sub>s</sub> Ψ does not hold.
     *               1. Else, Φ U<sub>s</sub> Ψ holds if and only if Φ U<sub>s+1</sub> Ψ holds.
     *                  <br/> '''N.B.:''' if Φ U<sub>s+1</sub> Ψ is Unknown then Φ U<sub>s</sub> Ψ does not hold,
     *                  since the [[FiniteStream FiniteStream]] ended with Ψ never being satisfied.
     *          - If '''s''' is '''terminal''':
     *            1. Complete the evaluation of Φ U<sub>s</sub> Ψ as Unknown, as evaluating the formula
     *               on an empty [[FiniteStream FiniteStream]] does not make sense.
     *     - If Φ U<sub>s0</sub> Ψ has '''already been determined''':
     *       1. Return, ignoring the evaluation of Φ U<sub>s</sub> Ψ as it is not required to determine
     *          Φ U<sub>s0</sub> Ψ.
     * @param initialEvaluation the evaluation of Φ U<sub>s0</sub> Ψ.
     * @return the recursive definition of the [[until]] operator, starting from the specified initial evaluation.
     */
    private def untilStep[S](initialEvaluation: AsyncEvaluation): BinaryLogicOperator[S] = (phi, psi) =>
      if initialEvaluation.isCompleted then
        Future.failed(CancellationException("Initial evaluation completed: evaluation on future state cancelled."))
      else
        evaluate(currentEvaluation =>
          val (phiEvaluation, psiEvaluation): (AsyncEvaluation, AsyncEvaluation) = (phi, psi)
          currentState[S] {
            case Event(_) =>
              val nextEvaluation: AsyncEvaluation = next(untilStep(initialEvaluation)(phi, psi))
              currentEvaluation.completeWith(
                psiEvaluation.flatMap(psiTruth =>
                  if psiTruth.isSatisfied then
                    Future.successful(psiTruth)
                  else
                    phiEvaluation.flatMap(phiTruth =>
                      if phiTruth.isFalsified then
                        Future.successful(False)
                      else
                        nextEvaluation.map { case Unknown => False; case b => b }
                    )
                )
              )
            case EOS =>
              currentEvaluation.success(Unknown)
          }
        )

    /**
     * Execute the specified property evaluator, evaluating a certain property.
     * The evaluator is given in input a [[Promise]] that can be completed
     * to complete the evaluation with a validity result for the property.
     *
     * @param evaluator the specified evaluator.
     * @return an [[AsyncEvaluation AsyncEvaluation]] completed by the specified
     *         evaluator.
     */
    private def evaluate(evaluator: Promise[Trilean] => Unit): AsyncEvaluation =
      val evaluation: Promise[Trilean] = Promise()
      evaluator(evaluation)
      evaluation.future

    /**
     * Register the specified consumer for the current state of the
     * given [[FiniteStream FiniteStream]].
     *
     * The current state of a [[FiniteStream FiniteStream]] is the
     * next event that it will fire.
     *
     * @param consumer the specified consumer.
     * @param scope    the given [[FiniteStream FiniteStream]].
     * @tparam E the type of the events generated by the given
     *           [[FiniteStream FiniteStream]].
     */
    private def currentState[E](consumer: FiniteEvent[E] => Unit)(using scope: FiniteStream[E]): Unit =
      stream[E].listenOnce(consumer(_))

    /**
     * @param scope the given [[FiniteStream FiniteStream]].
     * @tparam E the type of the events generated by the given [[FiniteStream FiniteStream]].
     * @return the given [[FiniteStream FiniteStream]] as the [[FiniteStream FiniteStream]]
     *         that is currently under evaluation.
     */
    private def stream[E](using scope: FiniteStream[E]): FiniteStream[E] = scope
  end FiniteStreamLinearTemporalLogic

  /** Companion object [[FiniteStreamLinearTemporalLogic]]. */
  object FiniteStreamLinearTemporalLogic:
    /**
     * A mixin for extending an [[FSLTL FSLTL]] with aliases and infix
     * notation to enhance the api experience and formula readability.
     *
     * @note includes [[LinearTemporalLogic.DSL]].
     */
    trait DSL extends LinearTemporalLogic.DSL { self: FSLTL => }

    /**
     * A mixin for extending a [[FSLTL FSLTL]] with operator
     * shorthands to increase the agility in defining formulas.
     *
     * @note includes [[LinearTemporalLogic.Shorthands]] and
     *       [[ThreeValueLogic.Shorthands]].
     */
    trait Shorthands extends LinearTemporalLogic.Shorthands with ThreeValueLogic.Shorthands { self: FSLTL => }
  end FiniteStreamLinearTemporalLogic
