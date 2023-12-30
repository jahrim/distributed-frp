package it.unibo.distributedfrp.simulation.sensors

import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation

/**
 * A neighbor [[Sensor]] in a FRASP simulation.
 * @tparam R the type of readings produced by the sensor.
 */
trait NeighborSensor[R] extends Sensor[R]:
  /**
   * Setup this [[Sensor]], so it can be used within the specified
   * [[SimulationIncarnation]].
   *
   * @param incarnation the specified [[SimulationIncarnation]].
   * @tparam I the type of the specified [[SimulationIncarnation]].
   * @return this [[Sensor]] adapted to the specified [[SimulationIncarnation]].
   */
  def setup[I <: SimulationIncarnation](incarnation: I)(using incarnation.Environment <:< SuitableEnvironment)
    : incarnation.SimulationNeighborSensor[R]
