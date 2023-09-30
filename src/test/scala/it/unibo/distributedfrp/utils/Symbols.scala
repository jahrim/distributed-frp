package it.unibo.distributedfrp.utils

/** A mixin that allows to create symbols within the inheriting class. */
trait Symbols:
  /**
   * @param name the specified name.
   * @return a symbol with the specified name.
   */
  def symbol(name: String): String = s"`$name`"
