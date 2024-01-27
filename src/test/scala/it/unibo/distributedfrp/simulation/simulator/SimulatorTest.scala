package it.unibo.distributedfrp.simulation.simulator

import it.unibo.distributedfrp.AbstractTest
import it.unibo.distributedfrp.core.ExportTree
import it.unibo.distributedfrp.frp.StreamExtension.Stream
import it.unibo.distributedfrp.simulation.environment.Environment

import scala.concurrent.Await

/** Test for [[StepSimulator]]. */
trait SimulatorTest extends AbstractTest:
  /**
   * A [[Simulator Simulator]] that can create and execute simulations with
   * a configuration fixed for testing.
   */
  protected trait SimulatorFixture:
    self: Simulator =>
    /** A [[Simulation Simulation]] with a configuration fixed for testing. */
    type SimulationFixture[A] <: Simulation[A]
    /**
     * @param flow        the specified [[Flow Flow]].
     * @param environment the specified [[Environment Environment]].
     * @tparam A the type of result produced by the [[Simulation Simulation]].
     * @return a [[Simulation Simulation]] that executes the specified [[Flow Flow]]
     *         in the specified [[Environment Environment]] with a configuration fixed
     *         for testing.
     */
    def createSimulationFixture[A](flow: incarnation.Flow[A], environment: Environment): SimulationFixture[A]

    /**
     * Execute and terminate the specified [[Simulation Simulation]].
     *
     * @param simulation the specified [[Simulation Simulation]].
     * @tparam A the type of result produced by the [[Simulation Simulation]].
     */
    def executeSimulationFixture[A](simulation: SimulationFixture[A]): Unit

  /**
   * A collection of tests defining the behavior of a [[Simulator Simulator]].
   *
   * @param simulator the [[SimulatorFixture SimulatorFixture]] to be tested.
   */
  protected def simulator[S <: Simulator](simulator: Simulator with SimulatorFixture): Unit =
    import simulator.incarnation.{*, given}

    def createDefaultSimulationFixture(): simulator.Simulation[Int] =
      simulator.createSimulationFixture(constant(0), Environment.singleNode)

    it should "create a simulation that is not running initially" in {
      createDefaultSimulationFixture().isRunning shouldBe false
    }

    it should "create simulations that can be started" in {
      val simulation = createDefaultSimulationFixture()
      simulation.start()
      simulation.isRunning shouldBe true
    }

    it should "create simulations that cannot be started twice" in {
      val simulation = createDefaultSimulationFixture()
      simulation.start()
      assertThrows[IllegalStateException]{ simulation.start() }
    }

    it should "create simulations that can be stopped" in {
      val simulation = createDefaultSimulationFixture()
      simulation.start()
      simulation.stop()
      simulation.isRunning shouldBe false
    }

    it should "create simulations that cannot be stopped twice" in {
      val simulation = createDefaultSimulationFixture()
      simulation.start()
      simulation.stop()
      assertThrows[IllegalStateException] { simulation.stop() }
    }

    it should "create simulations that cannot be stopped if not running" in {
      assertThrows[IllegalStateException] { createDefaultSimulationFixture().stop() }
    }

    it should "create simulations that notify the user of their termination" in {
      val simulation = createDefaultSimulationFixture()
      simulation.start()
      simulation.termination.isCompleted shouldBe false
      simulation.stop()
      simulation.termination.isCompleted shouldBe true
    }

    locally {
      val value = 0
      val environment = Environment.euclideanGrid(3, 3)
      val simulation = simulator.createSimulationFixture(constant(value), environment)

      val exportedByMonitor = Stream.monitor(simulation.exportedBy(0))
      val exportedMonitor = Stream.monitor(simulation.exported)
      val exportedByAllMonitor = Stream.monitor(simulation.exportedByAll)
      val computedByMonitor = Stream.monitor(simulation.computedBy(0))
      val computedMonitor = Stream.monitor(simulation.computed)
      val computedByAllMonitor = Stream.monitor(simulation.computedByAll)

      simulation.start()
      simulator.executeSimulationFixture(simulation)
      Await.ready(simulation.termination, Defaults.timeout)

      it should "create simulations that notify the user of the exports of a specific device in the system" in {
        exportedByMonitor.eventLog shouldEqual Seq(ExportTree(value))
      }
      it should "create simulations that notify the user of the exports of all the devices in the system" in {
        exportedMonitor.eventLog.toSet shouldEqual
          Set.range(0, environment.nDevices).map(id => Map(id -> ExportTree(value)))
      }
      it should "create simulations that notify the user of the exports of the collective system" in {
        exportedByAllMonitor.eventLog.last shouldEqual
          Set.range(0, environment.nDevices).map(_ -> ExportTree(value)).toMap
      }
      it should "create simulations that notify the user of the results of a specific device in the system" in {
        computedByMonitor.eventLog shouldEqual Seq(value)
      }
      it should "create simulations that notify the user of the results of all the devices in the system" in {
        computedMonitor.eventLog.toSet shouldEqual
          Set.range(0, environment.nDevices).map(id => Map(id -> value))
      }
      it should "create simulations that notify the user of the results of the collective system" in {
        computedByAllMonitor.eventLog.last shouldEqual
          Set.range(0, environment.nDevices).map(_ -> value).toMap
      }
    }
