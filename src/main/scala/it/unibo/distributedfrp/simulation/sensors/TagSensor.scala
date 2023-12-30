package it.unibo.distributedfrp.simulation.sensors

import it.unibo.distributedfrp.simulation.environment.EnvironmentWithTags
import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation

/**
 * A [[LocalSensor]] that perceives the presence of a certain tag
 * bound to the owner device.
 */
trait TagSensor extends LocalSensor[Boolean]:
  override type SuitableEnvironment <: EnvironmentWithTags

  /** The unique tag perceived by this [[TagSensor]]. */
  object Tag
  override def setup[I <: SimulationIncarnation](incarnation: I)(using incarnation.Environment <:< SuitableEnvironment):
    incarnation.SimulationLocalSensor[Boolean] = owner =>
    owner.environment.withTag(Tag).map(_.contains(owner.selfId))

/** Companion object of [[TagSensor]]. */
object TagSensor:
  /** The default configuration of a [[TagSensor]]. */
  trait Default extends TagSensor { override type SuitableEnvironment = EnvironmentWithTags }
