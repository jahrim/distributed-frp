package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.core.*

trait Incarnation extends Core, Language, Semantics, CoreExtensions, RichLanguage:
  def context(selfId: DeviceId): Context
