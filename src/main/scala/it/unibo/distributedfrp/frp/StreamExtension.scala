package it.unibo.distributedfrp.frp

import nz.sodium

/** An extension for [[sodium.Stream Stream]]s. */
object StreamExtension:
  extension[A] (self: Seq[A]){
    /**
     * @return the element of this singleton [[Seq]].
     * @throws IndexOutOfBoundsException if this [[Seq]] is empty.
     */
    private def toTuple1: A = self.head
    /**
     * @return a pair of the only two elements in this [[Seq]].
     * @throws IndexOutOfBoundsException if this [[Seq]] has less than two elements.
     */
    private def toTuple2: (A, A) = (self.head, self(1))
    /**
     * @return a triplet of the only three elements in this [[Seq]].
     * @throws IndexOutOfBoundsException if this [[Seq]] has less than three elements.
     */
    private def toTuple3: (A, A, A) = (self.head, self(1), self(2))
  }

  /** Companion object of [[sodium.Stream Stream]]. */
  object Stream:
    /**
     * A [[sodium.Stream Stream]] obtained by joining multiple [[sodium.Stream Stream]]s
     * together.
     *
     * At any given time, the events of those [[sodium.Stream Stream]]s are combined
     * into a [[Map]] from their identifiers to the respective events at that time, if any.
     *
     * The identifier of a [[sodium.Stream Stream]] is an index indicating its position in
     * the sequence of the [[sodium.Stream Stream]]s that were combined into this
     * [[JointStream JointStream]].
     */
    type JointStream[A] = sodium.Stream[Map[Int, A]]

    /**
     * Create a [[sodium.Stream Stream]] of all the events produced by the specified
     * [[sodium.Stream Stream]]s with the same type.
     *
     * @param streams the specified [[sodium.Stream Stream]]s.
     * @tparam A the type of events produced by the specified [[sodium.Stream Stream]]s.
     * @return a new [[JointStream JointStream]] which forwards all the events produced by
     *         the specified [[sodium.Stream Stream]]s.
     * @example {{{
     *   s1 [id: 0]: | a       b                            c
     *   s2 [id: 1]: |                  aa        bb        cc        dd
     *   or(s1,s2):  | (a, _)  (b, _)   (_, aa)   (_, bb)   (c, cc)   (_, dd)
     *   ---------------------------------------------------------------------------------------------> t
     * }}}
     * @example {{{
     *   s1 [id: 0]:   | a          b                                       c              d
     *   s2 [id: 1]:   |                        aa           bb             cc
     *   s3 [id: 2]:   |                                     aaa            bbb            ccc
     *   or(s1,s2,s3): | (a, _, _)  (b, _, _)   (_, aa, _)   (_, bb, aaa)   (c, cc, bbb)   (d, _, ccc)
     *   ---------------------------------------------------------------------------------------------> t
     * }}}
     */
    def or[A](streams: sodium.Stream[A]*): JointStream[A] =
      Stream.untypedOr(streams*).asInstanceOf[JointStream[A]]

    /** As [[Stream.or]], but it can be applied to [[sodium.Stream Stream]]s of different types. */
    def untypedOr(streams: sodium.Stream[?]*): JointStream[Any] =
      streams
        .zipWithIndex
        .map((xs, i) => xs.map((x: Any) => Map(i -> x)))
        .reduce((xs, ys) => xs.merge(ys, _ ++ _))

    /**
     * Create a [[sodium.Stream Stream]] of the simultaneous events produced by
     * all the specified [[sodium.Stream Stream]]s.
     *
     * @param streams the specified [[sodium.Stream Stream]]s.
     * @tparam A the type of events produced by the specified [[sodium.Stream Stream]]s.
     * @return a new [[JointStream JointStream]] which forwards the simultaneous events
     *         produced by all the specified [[sodium.Stream Stream]]s.
     * @example {{{
     *   s1 [id: 0]: | a   b           c
     *   s2 [id: 1]: |         1   2   3        4
     *   and(s1 s2): |                 (c, 3)
     *   -------------------------------------------------------> t
     * }}}
     * @example {{{
     *   s1 [id: 0]:    | a   b               c              d
     *   s2 [id: 1]:    |     aa   bb         cc
     *   s3 [id: 2]:    |               aaa   bbb            ccc
     *   and(s1,s2,s3): |                     (c, cc, bbb)
     *   -------------------------------------------------------> t
     * }}}
     */
    def and[A](streams: sodium.Stream[A]*): JointStream[A] =
      Stream.untypedAnd(streams*).asInstanceOf[JointStream[A]]

    /** As [[Stream.and]], but it can be applied to [[sodium.Stream Stream]]s of different types. */
    def untypedAnd(streams: sodium.Stream[?]*): JointStream[Any] =
      Stream.untypedOr(streams*)
        .map(Option(_).filter(_.size == streams.size))
        .defined

    /**
     * Synchronize the event production of the specified [[sodium.Stream Stream]]s.
     *
     * @param streams  the specified [[sodium.Stream Stream]]s.
     * @param memory the specified memory (default: unlimited).
     * @tparam A the type of events produced by the specified [[sodium.Stream Stream]]s.
     * @return a new [[JointStream JointStream]] obtained by synchronizing the events produced
     *         by the specified [[sodium.Stream Stream]]s.
     * @note   Synchronizing two source [[sodium.Stream Stream]]s means that each time the former
     *         produces an event, the new [[JointStream JointStream]] will produce a pair containing
     *         that event and the oldest event of the latter that hasn't been paired
     *         yet, and viceversa. The new [[JointStream JointStream]] produces an event only when that
     *         pairing is possible (i.e. both source [[sodium.Stream Stream]]s must have produced a
     *         yet unpaired event).
     *
     *         This process requires the new [[JointStream JointStream]] to keep track of the events
     *         of all the source [[sodium.Stream Stream]]s. In this context, the specified memory
     *         indicates how many events of each source [[sodium.Stream Stream]] will be remembered
     *         by the new [[JointStream JointStream]] before it starts dropping the oldest events of the
     *         source [[sodium.Stream Stream]]s.
     * @example {{{
     *   s1:             | a   b   c                   d                    e        f
     *   s2:             |             aa      bb      cc      dd      ee            ff
     *   sync(s1,s2)():  |             (a,aa)  (b,bb)  (c,cc)  (d,dd)       (e,ee)   (f,ff)
     *   sync(s1,s2)(1): |             (c,aa)          (d,cc)               (e,ee)   (f,ff)
     *   sync(s1,s2)(2): |             (b,aa)  (c,bb)  (d,cc)               (e,dd)   (f,ee)
     *   ----------------------------------------------------------------------------------------------> t
     * }}}
     * @example {{{
     *   s1:                | a b c       d                                     e       f       g
     *   s2:                |       j k l m                                 n o                 p
     *   s3:                |               t       u       v       w       x           y       z
     *   sync(s1,s2,s3)():  |               (a,j,t) (b,k,u) (c,l,v) (d,m,w)     (e,n,x) (f,o,y) (g,p,z)
     *   sync(s1,s2,s3)(1): |               (d,m,t)                             (e,o,x)         (g,p,z)
     *   sync(s1,s2,s3)(2): |               (c,l,t) (d,m,u)                     (e,n,w) (f,o,x) (g,p,y)
     *   ----------------------------------------------------------------------------------------------> t
     * }}}
     */
    def sync[A](streams: sodium.Stream[A]*)(memory: Int = Int.MaxValue): JointStream[A] =
      Stream.untypedSync(streams*)(memory).asInstanceOf[JointStream[A]]

    /** As [[Stream.sync]], but it can be applied to [[sodium.Stream Stream]]s of different types. */
    def untypedSync(streams: sodium.Stream[?]*)(memory: Int = Int.MaxValue): JointStream[Any] =
      Stream.untypedOr(streams*)
        .collectLazy(streams.indices.map(_ -> Seq.empty[Any]).toMap)((nextEvents, eventQueues) =>
          val nextEventQueues: Map[Int, Seq[Any]] =
            eventQueues
              .map((sourceId, eventQueue) => sourceId ->
                nextEvents
                  .get(sourceId)
                  .map(eventQueue :+ _)
                  .map(_.takeRight(memory))
                  .getOrElse(eventQueue)
              )
          if nextEventQueues.values.forall(_.nonEmpty) then
            (Option(nextEventQueues.map(_ -> _.head)), nextEventQueues.map(_ -> _.drop(1)))
          else
            (Option.empty[Map[Int, Any]], nextEventQueues)
        )
        .defined

    /**
     * Start monitoring the specified [[sodium.Stream Stream]], recording its events using a
     * new [[StreamMonitor StreamMonitor]].
     *
     * @param stream the specified [[sodium.Stream Stream]].
     * @param memory the memory of the [[StreamMonitor StreamMonitor]] (default: unlimited).
     *               The memory indicates the number of events the [[StreamMonitor StreamMonitor]]
     *               will keep track of before discarding the oldest ones.
     * @tparam A the type of events produced by the specified [[sodium.Stream Stream]].
     * @tparam S the type of the specified [[sodium.Stream Stream]].
     * @return the specified [[sodium.Stream Stream]] itself and a new [[StreamMonitor StreamMonitor]]
     *         recording its events.
     */
    def monitor[A, S <: sodium.Stream[A]](stream: S, memory: Int = Int.MaxValue): (S, StreamMonitor[A, S]) =
      (stream, StreamMonitor(stream, memory))
  end Stream

  extension[A] (self: sodium.Stream[Option[A]]) {
    /**
     * @return a new [[sodium.Stream Stream]] obtained by filtering the [[Some]]s
     *         out of this [[sodium.Stream Stream]] and extracting their contents.
     * @example {{{
     *   s:         | None   Some(1)   Some(2)   None   None   Some(3)
     *   s.defined: |        1         2                       3
     *   -------------------------------------------------------------> t
     * }}}
     */
    def defined: sodium.Stream[A] = self.filter(_.isDefined).map(_.get)
  }

  extension[A] (self: sodium.Stream[A]) {
    /**
     * Transform an event [[sodium.Stream Stream]] with a generalized state loop
     * (a Mealy machine).
     *
     * @param initialState the initial state of the Mealy machine.
     * @param collector    a function that consumes the previous state and the latest event of
     *                     this [[sodium.Stream Stream]] to produce a new state and event. The
     *                     function must be <em>referentially transparent</em>.
     * @tparam B the type of events produced by the function.
     * @tparam S the type of states produced by the function.
     * @return a new [[sodium.Stream Stream]] of the produced events.
     * @note this is just a refactoring of the original function, adapted to Scala 3.
     */
    def collectLazy[B, S](initialState: => S)(collector: (A, S) => (B, S)): sodium.Stream[B] =
      self.collectLazy(sodium.Lazy(initialState), (next, state) =>
        val (newEvent, newState) = collector(next, state)
        sodium.Tuple2(newEvent, newState)
      )

    /**
     * As [[collectLazy collectLazy(init)(collector)]], but the next state produced by the
     * collector is also the next element of the new [[sodium.Stream Stream]].
     */
    def fold[B](init: => B)(accumulator: (B, A) => B): sodium.Stream[B] =
      self.collectLazy(init)((next, state) => {
        val acc: B = accumulator(state, next)
        (acc, acc)
      })

    /**
     * @return a new [[sodium.Stream Stream]] containing pairs binding each event of
     *         this [[sodium.Stream Stream]] to a number indicating the discrete time
     *         when the event was received.
     * @example {{{
     *   s:               | a       b       c       d       e
     *   s.zipWithIndex:  | (a,0)   (b,1)   (c,2)   (d,3)   (b,4)
     *   ------------------------------------------------------------> t
     * }}}
     */
    def zipWithIndex: sodium.Stream[(A, Int)] =
      self.collectLazy(0)((next, state) => ((next, state), state + 1))

    /**
     * Create a new [[sodium.Stream Stream]] which keeps track of the events
     * generated by this [[sodium.Stream Stream]].
     *
     * @param memory the specified memory (default: unlimited). The memory
     *               indicates the number of events the new [[sodium.Stream Stream]]
     *               will keep track of before discarding the oldest ones.
     * @return a new [[sodium.Stream Stream]] which keeps track of the events
     *         generated by this [[sodium.Stream Stream]].
     * @example {{{
     *   s:         | a     b       c         d           e
     *   s.cold():  | [a]   [a,b]   [a,b,c]   [a,b,c,d]   [a,b,c,d,e]
     *   s.cold(2): | [a]   [a,b]   [b,c]     [c,d]       [d,e]
     *   s.cold(3): | [a]   [a,b]   [a,b,c]   [b,c,d]     [c,d,e]
     *   ------------------------------------------------------------> t
     * }}}
     */
    def cold(memory: Int = Int.MaxValue): sodium.Stream[Seq[A]] =
      self.fold(Seq.empty[A])((acc, next) => (acc :+ next).takeRight(memory))

    /**
     * Group the events produced by this [[sodium.Stream Stream]] into groups of the
     * specified size.
     *
     * @param n the specified size.
     * @return a new [[sodium.Stream Stream]] obtained by grouping the events of this
     *         [[sodium.Stream Stream]] into groups of the specified size.
     * @note if not enough events are present in this [[sodium.Stream Stream]], then
     *       incomplete groups will be produced. Contrary to the [[ngrams]] variant,
     *       this method won't discard incomplete groups.
     * @example {{{
     *   s:                 | a        b        c        d        e
     *   s.ngramsOption(1): | [a]      [b]      [c]      [d]      [e]
     *   s.ngramsOption(2): | [_,a]    [a,b]    [b,c]    [c,d]    [d,e]
     *   s.ngramsOption(3): | [_,_,a]  [_,a,b]  [a,b,c]  [b,c,d]  [c,d,e]
     *   ----------------------------------------------------------------> t
     * }}}
     */
    def ngramsOption(n: Int): sodium.Stream[Seq[Option[A]]] =
      self.fold(Seq.fill(n)(Option.empty[A]))((acc, next) => acc.drop(1) :+ Some(next))
    /** As [[ngramsOption ngramsOption(1)]]. */
    def unigramsOption: sodium.Stream[Option[A]] =
      self.ngramsOption(1).map(_.toTuple1)
    /** As [[ngramsOption ngramsOption(2)]]. */
    def bigramsOption: sodium.Stream[(Option[A], Option[A])] =
      self.ngramsOption(2).map(_.toTuple2)
    /** As [[ngramsOption ngramsOption(3)]]. */
    def trigramsOption: sodium.Stream[(Option[A], Option[A], Option[A])] =
      self.ngramsOption(3).map(_.toTuple3)

    /**
     * Group the events produced by this [[sodium.Stream Stream]] into groups of the
     * specified size.
     *
     * @param n the specified size.
     * @return a new [[sodium.Stream Stream]] obtained by grouping the events of this
     *         [[sodium.Stream Stream]] into groups of the specified size.
     * @note if not enough events are present in this [[sodium.Stream Stream]], then
     *       incomplete groups will be produced. Contrary to the [[ngramsOption]] variant,
     *       this method will discard incomplete groups.
     * @example {{{
     *   s:           | a    b      c        d        e
     *   s.ngrams(1): | [a]  [b]    [c]      [d]      [e]
     *   s.ngrams(2): |      [a,b]  [b,c]    [c,d]    [d,e]
     *   s.ngrams(3): |             [a,b,c]  [b,c,d]  [c,d,e]
     *   ----------------------------------------------------> t
     * }}}
     */
    def ngrams(n: Int): sodium.Stream[Seq[A]] =
      self.ngramsOption(n).filter(_.forall(_.isDefined)).map(_.map(_.get))

    /** As [[ngrams ngrams(1)]]. */
    def unigrams: sodium.Stream[A] =
      self.ngrams(1).map(_.toTuple1)

    /** As [[ngrams ngrams(2)]]. */
    def bigrams: sodium.Stream[(A, A)] =
      self.ngrams(2).map(_.toTuple2)

    /** As [[ngrams ngrams(3)]]. */
    def trigrams: sodium.Stream[(A, A, A)] =
      self.ngrams(3).map(_.toTuple3)

    /**
     * Create a [[sodium.Stream Stream]] of the updates of this
     * [[sodium.Stream Stream]].
     *
     * @return a new [[sodium.Stream Stream]] obtained by discarding all
     *         consecutively repeated events in this [[sodium.Stream Stream]]
     *         (i.e. considering only the events that are new with respect to
     *         the latest event of [[sodium.Stream Stream]]).
     * @example {{{
     *   s:         | a   a   a   b   b   a   c   b   a   a   b
     *   s.updates: | a           b       a   c   b   a       b
     *   ------------------------------------------------------> t
     * }}}
     */
    def updates: sodium.Stream[A] =
      self
        .bigramsOption
        .filter((prev, next) => prev != next)
        .map(_._2)
        .defined

    /** Alias for [[updates]]. */
    def calm: sodium.Stream[A] = self.updates

    /**
     * As [[Stream.or Stream.or(this, other)]], but it produces a [[sodium.Stream Stream]]
     * of pairs containing the events of this [[sodium.Stream Stream]], the specified
     * [[sodium.Stream Stream]] or both of them in the case of simultaneous events.
     */
    def or[B](other: sodium.Stream[B]): sodium.Stream[(Option[A], Option[B])] =
      Stream.untypedOr(self, other)
        .map(ab => (ab.get(0), ab.get(1)))
        .asInstanceOf[sodium.Stream[(Option[A], Option[B])]]

    /**
     * As [[Stream.and Stream.and(this, other)]], but it produces a [[sodium.Stream Stream]]
     * of pairs containing the simultaneous events of this [[sodium.Stream Stream]] and the
     * specified [[sodium.Stream Stream]].
     */
    def and[B](other: sodium.Stream[B]): sodium.Stream[(A, B)] =
      Stream.untypedAnd(self, other)
        .map(ab => (ab(0), ab(1)))
        .asInstanceOf[sodium.Stream[(A, B)]]

    /**
     * As [[Stream.sync Stream.sync(this, other)(memory)]], but it produces a
     * [[sodium.Stream Stream]] of pairs containing the synced events of this
     * [[sodium.Stream Stream]] and the specified [[sodium.Stream Stream]].
     */
    def sync[B](other: sodium.Stream[B], memory: Int = Int.MaxValue): sodium.Stream[(A, B)] =
      Stream.untypedSync(self, other)(memory)
        .map(ab => (ab(0), ab(1)))
        .asInstanceOf[sodium.Stream[(A, B)]]

    /**
     * Limit the rate at which this [[sodium.Stream Stream]] produces events
     * to the rate determined by the specified [[sodium.Stream Stream]], acting
     * as a throttler.
     *
     * @param throttler the specified throttler.
     * @tparam B the type of events produced by the specified throttler.
     * @return a new [[sodium.Stream Stream]] obtained by pairing the latest event of
     *         this [[sodium.Stream Stream]] with the latest event of the specified
     *         [[sodium.Stream Stream]], if they haven't been paired yet.
     * @note this is equivalent to [[sync sync(throttler, 1)]], therefore
     *       remembering only the latest event of both [[sodium.Stream Stream]]s.
     * @see if the events of the throttler aren't relevant, consider using
     *      [[throttledBy throttledBy(throttler)]].
     */
    def withThrottler[B](throttler: sodium.Stream[B]): sodium.Stream[(A, B)] =
      self.sync(throttler, memory = 1)

    /**
     * As [[withThrottler withThrottler(throttler)]], but it ignores the content
     * of the events generated by the throttler.
     */
    def throttledBy(throttler: sodium.Stream[?]): sodium.Stream[A] =
      self.withThrottler(throttler).map(_._1)
  }
