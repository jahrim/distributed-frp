package it.unibo.distributedfrp.incarnation

import it.unibo.distributedfrp.core.*

trait Incarnation extends Core, Language, Semantics, CoreExtensions:
  def context(selfId: DeviceId): Context
