package it.unibo.distributedfrp.incarnation

import it.unibo.distributedfrp.core.*

trait Incarnation extends Core, Language, Semantics, CoreExtensions:
  trait Program[A]:
    def main(using Context): Flow[A]
