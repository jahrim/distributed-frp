package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.core.*

trait Incarnation extends Core, Language, SemanticsModel, ConstructsSemantics, RichLanguage:
  def context(selfId: DeviceId): Context
