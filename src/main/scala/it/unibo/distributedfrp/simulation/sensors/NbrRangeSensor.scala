package it.unibo.distributedfrp.simulation.sensors

import it.unibo.distributedfrp.simulation.environment.Environment
import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation

/**
 * A [[NeighborSensor]] that perceives the distance between the
 * owner device and each of its neighbors.
 */
object NbrRangeSensor extends NeighborSensor[Double]:
  override type SuitableEnvironment = Environment
  override def setup[I <: SimulationIncarnation](incarnation: I)(using incarnation.Environment <:< SuitableEnvironment):
    incarnation.SimulationNeighborSensor[Double] = neighborState =>
    val selfPos = neighborState.environment.position(neighborState.selfId)
    val neighborPos = neighborState.environment.position(neighborState.neighborId)
    math.hypot(selfPos._1 - neighborPos._1, selfPos._2 - neighborPos._2)
