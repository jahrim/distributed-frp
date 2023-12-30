package it.unibo.distributedfrp.simulation.sensors

import it.unibo.distributedfrp.simulation.environment.Environment

/**
 * A sensor producing certain readings from the environment.
 * @tparam R the type of readings produced by the sensor.
 */
trait Sensor[R]:
  /** The type of [[Environment]] where this [[Sensor]] can be used. */
  type SuitableEnvironment <: Environment
