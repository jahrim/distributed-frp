package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.frp.FiniteStreamExtension.*
import it.unibo.distributedfrp.frp.FiniteStreamExtension.FiniteEvent.*
import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.simulation.simulator.{BasicSimulator, Simulator}
import it.unibo.distributedfrp.simulation.{Environment, SimulationIncarnation}
import it.unibo.distributedfrp.test.utils.logic.fol.FirstOrderLogic
import it.unibo.distributedfrp.test.utils.logic.fsltl.FSLTLExtension.*
import it.unibo.distributedfrp.test.utils.logic.trilean.PriestLogic
import it.unibo.distributedfrp.utils.GridView
import it.unibo.distributedfrp.utils.Liftable.map

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

/** A showcase of [[FSLTL]] for checking temporal properties on [[FiniteStream FiniteStream]]s. */
object FSLTLShowcase:

  /** An examination on the results computed by a single device. */
  @main def individualExamination(): Unit =
    /** Prepare simulator. */
    val environment = Environment.euclideanGrid(3, 3)
    val incarnation = new SimulationIncarnation(environment)
    val simulator: Simulator[SimulationIncarnation] = BasicSimulator(incarnation)
    import simulator.{*, given}

    /** Prepare simulation. */
    def countToInfinite: Flow[Int] = loop(0)(_.map(_ + 1))
    val simulation: Simulation[Int] = simulator.simulation(countToInfinite)
    val computation: FiniteStream[Int] = simulation.computedBy(deviceId = 0).finite.take(10)
    val computationMonitor = Stream.monitor(computation)

    /** Prepare logic. */
    given ExecutionContext = ExecutionContext.global
    object DeclarativeLTL extends FSLTL with FSLTL.DSL with PriestLogic
    import DeclarativeLTL.*
    // object ImperativeLTL extends FSLTL with PriestLogic
    // import ImperativeLTL.*
    // object MathematicalLTL extends FSLTL with FSLTL.Shorthands with PriestLogic
    // import MathematicalLTL.*

    /** Prepare custom propositions. */
    object CustomPropositions:
      val even: Formula[Int] = predicate[Int](x => proposition(x % 2 == 0))
      val odd: Formula[Int] = not(even)
      def is(n: Int): Formula[Int] = predicate[Int](x => proposition(x == n))
      def lt(n: Int): Formula[Int] = predicate[Int](x => proposition(x < n))
      def gteq(n: Int): Formula[Int] = not(lt(n))
    import CustomPropositions.*

    /** Configure the evaluation of custom properties. */
    val alternatingParity: AsyncEvaluation =
      computation.check(using DeclarativeLTL)(always((even implies next(odd)) and (odd implies next(even))))
      // computation.check(using ImperativeLTL)(always(and(conditional(even, next(odd)), conditional(odd, next(even)))))
      // computation.check(using MathematicalLTL)(G((even ---> X(odd)) && (odd ---> X(even))))
    val increasingWithStep1: AsyncEvaluation =
      computation.check(using DeclarativeLTL)(always(let[Int](x => next(is(x + 1)))))
      // computation.check(using ImperativeLTL)(always(predicate[Int](x => next(predicate[Int](y => proposition(y == x + 1))))))
      // computation.check(using MathematicalLTL)(G(P[Int](x => X(is(x + 1)))))
    val sometimesEvenAndGreaterThan100: AsyncEvaluation =
      computation.check(using DeclarativeLTL)(sometimes(even and gteq(100)))
      // computation.check(using ImperativeLTL)(sometimes(and(even, gteq(100))))
      // computation.check(using MathematicalLTL)(F(even && gteq(100)))
    val evaluations: Seq[AsyncEvaluation] = Seq(alternatingParity, increasingWithStep1, sometimesEvenAndGreaterThan100)

    /** Run simulation. */
    simulation.start()
    Await.result(Future.sequence(evaluations), 10.seconds)
    println(s"# Event Log:")
    println(s"${computationMonitor.eventLog}")
    println(s"# Properties:")
    println(s"- Always alternating parity: ${alternatingParity.value}")
    println(s"- Always increasing with step 1: ${increasingWithStep1.value}")
    println(s"- Sometimes both even and greater than 100: ${sometimesEvenAndGreaterThan100.value}")
    simulation.stop()

  /** An examination on the results computed by a group of devices. */
  @main def groupExamination(): Unit =
    /** Prepare simulator. */
    val environment = Environment.euclideanGrid(3, 3)
    val incarnation = new SimulationIncarnation(environment)
    val simulator: Simulator[SimulationIncarnation] = BasicSimulator(incarnation)
    import simulator.{*, given}

    /** Prepare simulation. */
    def collectSortedNeighbors: Flow[Seq[DeviceId]] =
      nbr(mid).map(_.foldLeft(Seq[DeviceId]())(_ :+ _._1).sorted)
    val simulation: Simulation[Seq[DeviceId]] =
      simulator.simulation(collectSortedNeighbors)
    val computation: FiniteStream[CollectiveResultMap[Seq[DeviceId]]] =
      simulation.computedByAll.finite.interruptAfterInactivityOf(1.seconds)

    /** Prepare logic. */
    given ExecutionContext = ExecutionContext.global
    object LTL extends FSLTL with PriestLogic with FSLTL.DSL with FSLTL.Shorthands with FirstOrderLogic.Comparisons:
      /** Prepare custom propositions. */
      def convergeTo[S](s: S): Formula[S] = sometimes(always(is(s)))
    import LTL.*

    /** Configure the evaluation of custom properties. */
    val expectation: CollectiveResultMap[Seq[DeviceId]] = Map(
      0 -> Seq(0, 1, 3, 4),          1 -> Seq(0, 1, 2, 3, 4, 5),             2 -> Seq(1, 2, 4, 5),
      3 -> Seq(0, 1, 3, 4, 6, 7),    4 -> Seq(0, 1, 2, 3, 4, 5, 6, 7, 8),    5 -> Seq(1, 2, 4, 5, 7, 8),
      6 -> Seq(3, 4, 6, 7),          7 -> Seq(3, 4, 5, 6, 7, 8),             8 -> Seq(4, 5, 7, 8),
    )
    val convergence: AsyncEvaluation = computation.check(using LTL)(convergeTo(expectation))

    /** Run simulation. */
    simulation.start()
    Await.result(convergence, 10.seconds)
    println(s"# Expectation:")
    GridView.fixedWithIndex(expectation, 3, 3)(using _._1).show()
    println(s"# Properties:")
    println(s"- Convergence to expectation: ${convergence.value}")
    simulation.stop()

  @main def stopSimulationAfterEvaluationASAP(): Unit =
    /** Prepare simulator. */
    val environment = Environment.euclideanGrid(3, 3)
    val incarnation = new SimulationIncarnation(environment)
    val simulator: Simulator[SimulationIncarnation] = BasicSimulator(incarnation)
    import simulator.{*, given}

    /** Prepare simulation. */
    def countToInfinite: Flow[Int] = loop(0)(_.map(_ + 1))
    val simulation: Simulation[Int] = simulator.simulation(countToInfinite)
    val computation: FiniteStream[Int] = simulation.computedBy(0).finite.take(10000)
    val computationMonitor = Stream.monitor(computation, memory = 1)

    /** Prepare logic. */
    given ExecutionContext = ExecutionContext.global
    object LTL extends FSLTL with PriestLogic with FSLTL.DSL with FSLTL.Shorthands with FirstOrderLogic.Comparisons
    import LTL.*

    /** Configure the evaluation of custom properties. */
    val alwaysLessThan100: AsyncEvaluation = computation.check(using LTL)(always(lt(100)))

    /** Run simulation. */
    computation.listen(event => println(event))
    simulation.start()
    Await.result(alwaysLessThan100, 10.seconds)
    simulation.stop()   // commenting this will keep the simulation going after the evaluation
    println(s"# Properties:")
    println(s"- Always less than 100: ${alwaysLessThan100.value}")
    println(s"# Latest Computation After Failure:")
    println(computationMonitor.eventLog.last)

  @main def stopInfiniteSimulationAfterEvaluationASAP(): Unit =
    /** Prepare simulator. */
    val environment = Environment.euclideanGrid(3, 3)
    val incarnation = new SimulationIncarnation(environment)
    val simulator: Simulator[SimulationIncarnation] = BasicSimulator(incarnation)
    import simulator.{*, given}

    /** Prepare simulation. */
    def countToInfinite: Flow[Int] = loop(0)(_.map(_ + 1))
    val simulation: Simulation[Int] = simulator.simulation(countToInfinite)
    // only potentially finite, but actually infinite (no termination condition is specified)
    val computation: FiniteStream[Int] = simulation.computedBy(0).finite
    val computationMonitor = Stream.monitor(computation, memory = 1)

    /** Prepare logic. */
    given ExecutionContext = ExecutionContext.global

    object LTL extends FSLTL with PriestLogic with FSLTL.DSL with FSLTL.Shorthands with FirstOrderLogic.Comparisons
    import LTL.*

    /** Configure the evaluation of custom properties. */
    val alwaysLessThan100: AsyncEvaluation = computation.check(using LTL)(always(lt(100)))

    /** Run simulation. */
    computation.listen(event => println(event))
    simulation.start()
    Await.result(alwaysLessThan100, 10.seconds)
    simulation.stop() // commenting this will keep the simulation going forever after the evaluation
    println(s"# Properties:")
    println(s"- Always less than 100: ${alwaysLessThan100.value}")
    println(s"# Latest Computation After Failure:")
    println(computationMonitor.eventLog.last)
