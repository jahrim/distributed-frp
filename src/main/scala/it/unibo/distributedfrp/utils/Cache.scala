package it.unibo.distributedfrp.utils

/** Utilities for handling function caching. */
object Cache:
  /**
   * @param f the specified function.
   * @tparam A the type of the input of the specified function.
   * @tparam B the type of the output of the specified function.
   * @return a new function applying the logic of the specified
   *         function, storing previous evaluations for
   *         optimizations on future inputs.
   * @note the specified function should be referentially
   *       transparent for the cache to be meaningful.
   */
  def cached[A, B](f: A => B): A => B = new Function[A, B]:
    private var _cache: Map[A, B] = Map()
    override def apply(a: A): B =
      val b: B = this._cache.getOrElse(a, f(a))
      synchronized { this._cache = this._cache + (a -> b) }
      b
