package it.unibo.distributedfrp.samples

import it.unibo.distributedfrp.simulation.environment.{Environment, EnvironmentWithTags}
import it.unibo.distributedfrp.simulation.incarnation.{SimulationIncarnation, CommonSensors}
import it.unibo.distributedfrp.simulation.simulator.LegacySimulator
import it.unibo.distributedfrp.utils.Liftable.*

@main def reEvaluationSample(): Unit =
  given environment: EnvironmentWithTags = EnvironmentWithTags(Environment.singleNode)
  object Incarnation extends SimulationIncarnation with CommonSensors.Default
  val simulator = LegacySimulator(Incarnation)

  import simulator.incarnation.{*, given}

  def someIntenseComputation(input: String): String =
    println("Doing some intense computation...")
    input

  simulator.run {
    branch(isSource) {
      constant("I'm a source device").map(someIntenseComputation)
    } {
      constant("I'm not a source device")
    }
  }
                                             // "I'm not a source device"
  environment.tag(SourceTag, Set(0))         // "I'm a source device"
  environment.untag(SourceTag, Set(0))       // "I'm not a source device"
  environment.tag(SourceTag, Set(0))         // "I'm a source device"
  environment.untag(SourceTag, Set(0))       // "I'm not a source device"
