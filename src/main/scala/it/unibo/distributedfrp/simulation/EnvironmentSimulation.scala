package it.unibo.distributedfrp.simulation

import it.unibo.distributedfrp.core.Incarnation

trait EnvironmentSimulation(val environment: Environment):
  self: Incarnation =>

  override type DeviceId = Int
