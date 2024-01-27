package it.unibo.distributedfrp.utils

import it.unibo.distributedfrp.AbstractTest

/** Test for [[Cache]]. */
class CacheTest extends AbstractTest:
  private val Cached = symbol("cached")

  Cached should
    "create a function that avoids recomputing the outputs " +
    "for inputs that were already observed" in {
    var functionCalls: Int = 0
    val cachedFunction: Int => Int = Cache.cached { input => functionCalls = functionCalls + 1; functionCalls }

    // New inputs: `functionCalls` is updated
    cachedFunction(0) shouldEqual 1
    cachedFunction(1) shouldEqual 2
    cachedFunction(2) shouldEqual 3

    // Cached outputs: `functionCalls` is not updated
    cachedFunction(0) shouldEqual 1
    cachedFunction(1) shouldEqual 2
    cachedFunction(2) shouldEqual 3

    // New inputs: `functionCalls` is updated from the latest new input
    cachedFunction(3) shouldEqual 4
  }
