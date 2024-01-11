package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.utils.Clock
import nz.sodium

import scala.concurrent.duration.*

/** A general extension for [[sodium.Stream Stream]]s. */
object StreamExtension:
  /** A stream of events. */
  type Stream[A] = sodium.Stream[A]

  /**
   * A [[Stream Stream]] obtained by joining multiple [[Stream Stream]]s
   * together.
   *
   * At any given time, the events of those [[Stream Stream]]s are combined
   * into a [[Map]] from their identifiers to the respective events at that time,
   * if any.
   *
   * The identifier of a [[Stream Stream]] is an index indicating its position in
   * the ordered sequence of the [[Stream Stream]]s that were combined into this
   * [[JointStream JointStream]].
   */
  type JointStream[A] = Stream[Map[Int, A]]

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

  /** Companion object of [[Stream Stream]]. */
  object Stream:
    /**
     * Create a [[Stream Stream]] of all the events produced by the specified
     * [[Stream Stream]]s with the same type.
     *
     * @param streams the specified [[Stream Stream]]s.
     * @tparam A the type of events produced by the specified [[Stream Stream]]s.
     * @return a new [[JointStream JointStream]] which forwards all the events produced by
     *         the specified [[Stream Stream]]s.
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
    def or[A](streams: Stream[A]*): JointStream[A] =
      Stream.untypedOr(streams*).asInstanceOf[JointStream[A]]

    /** As [[Stream.or]], but it can be applied to [[Stream Stream]]s of different types. */
    def untypedOr(streams: Stream[?]*): JointStream[Any] =
      streams
        .zipWithIndex
        .map((xs, i) => xs.map((x: Any) => Map(i -> x)))
        .reduce((xs, ys) => xs.merge(ys, _ ++ _))

    /**
     * Create a [[Stream Stream]] of the simultaneous events produced by
     * all the specified [[Stream Stream]]s.
     *
     * @param streams the specified [[Stream Stream]]s.
     * @tparam A the type of events produced by the specified [[Stream Stream]]s.
     * @return a new [[JointStream JointStream]] which forwards the simultaneous events
     *         produced by all the specified [[Stream Stream]]s.
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
    def and[A](streams: Stream[A]*): JointStream[A] =
      Stream.untypedAnd(streams*).asInstanceOf[JointStream[A]]

    /** As [[Stream.and]], but it can be applied to [[Stream Stream]]s of different types. */
    def untypedAnd(streams: Stream[?]*): JointStream[Any] =
      Stream.untypedOr(streams*)
        .map(Option(_).filter(_.size == streams.size))
        .defined

    /**
     * Synchronize the event production of the specified [[Stream Stream]]s.
     *
     * @param streams  the specified [[Stream Stream]]s.
     * @param memory the specified memory (default: unlimited).
     * @tparam A the type of events produced by the specified [[Stream Stream]]s.
     * @return a new [[JointStream JointStream]] obtained by synchronizing the events produced
     *         by the specified [[Stream Stream]]s.
     * @note   Synchronizing two source [[Stream Stream]]s means that each time the former
     *         produces an event, the new [[JointStream JointStream]] will produce a pair containing
     *         that event and the oldest event of the latter that hasn't been paired
     *         yet, and viceversa. The new [[JointStream JointStream]] produces an event only when that
     *         pairing is possible (i.e. both source [[Stream Stream]]s must have produced a
     *         yet unpaired event).
     *
     *         This process requires the new [[JointStream JointStream]] to keep track of the events
     *         of all the source [[Stream Stream]]s. In this context, the specified memory
     *         indicates how many events of each source [[Stream Stream]] will be remembered
     *         by the new [[JointStream JointStream]] before it starts dropping the oldest events of the
     *         source [[Stream Stream]]s.
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
    def sync[A](streams: Stream[A]*)(memory: Int = Int.MaxValue): JointStream[A] =
      Stream.untypedSync(streams*)(memory).asInstanceOf[JointStream[A]]

    /** As [[Stream.sync]], but it can be applied to [[Stream Stream]]s of different types. */
    def untypedSync(streams: Stream[?]*)(memory: Int = Int.MaxValue): JointStream[Any] =
      Stream.untypedOr(streams*)
        .collect(streams.indices.map(_ -> Seq.empty[Any]).toMap)((nextEvents, eventQueues) =>
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
     * Start monitoring the specified [[Stream Stream]], recording its events using a
     * new [[StreamMonitor StreamMonitor]].
     *
     * @param stream the specified [[Stream Stream]].
     * @param memory the memory of the [[StreamMonitor StreamMonitor]] (default: unlimited).
     *               The memory indicates the number of events the [[StreamMonitor StreamMonitor]]
     *               will keep track of before discarding the oldest ones.
     * @tparam A the type of events produced by the specified [[Stream Stream]].
     * @tparam S the type of the specified [[Stream Stream]].
     * @return a new [[StreamMonitor StreamMonitor]] recording the events of the specified
     *         [[Stream Stream]].
     */
    def monitor[A, S <: Stream[A]](stream: S, memory: Int = Int.MaxValue): StreamMonitor[A, S] =
      StreamMonitor(stream, memory)
  end Stream

  extension[A] (self: Stream[Option[A]]) {
    /**
     * @return a new [[Stream Stream]] obtained by filtering the [[Some]]s
     *         out of this [[Stream Stream]] and extracting their contents.
     * @example {{{
     *   s:         | None   Some(1)   Some(2)   None   None   Some(3)
     *   s.defined: |        1         2                       3
     *   -------------------------------------------------------------> t
     * }}}
     */
    def defined: Stream[A] = self.filter(_.isDefined).map(_.get)
  }

  extension[A] (self: Stream[A]) {
    /**
     * Transform an event [[Stream Stream]] with a generalized state loop
     * (a Mealy machine).
     *
     * @param initialState the initial state of the Mealy machine.
     * @param collector    a function that consumes the previous state and the latest event of
     *                     this [[Stream Stream]] to produce a new state and event. The
     *                     function must be <em>referentially transparent</em>.
     * @tparam B the type of events produced by the function.
     * @tparam S the type of states produced by the function.
     * @return a new [[Stream Stream]] of the produced events.
     * @note this is just a refactoring of the original function, adapted to Scala 3.
     */
    def collect[B, S](initialState: => S)(collector: (A, S) => (B, S)): Stream[B] =
      self.collect(initialState, (next, state) =>
        val (newEvent, newState) = collector(next, state)
        sodium.Tuple2(newEvent, newState)
      )

    /**
     * As [[collect collect(init)(collector)]], but the next state produced by the
     * collector is also the next element of the new [[Stream Stream]].
     */
    def fold[B](init: => B)(accumulator: (B, A) => B): Stream[B] =
      self.collect(init)((next, state) => {
        val acc: B = accumulator(state, next)
        (acc, acc)
      })

    /**
     * @param initialIndex the specified initial index. Defaults to 0.
     * @return a new [[Stream Stream]] containing pairs binding each event of
     *         this [[Stream Stream]] to a number indicating the instant in the
     *         discrete timeline when the event was received, starting from the specified
     *         initial index.
     * @note see [[zipWithTime]] for dealing with continuous time instead.
     * @example {{{
     *   s:                  | a        b        c        d        e
     *   s.zipWithIndex():   | (a,0)    (b,1)    (c,2)    (d,3)    (e,4)
     *   s.zipWithIndex(10): | (a,10)   (b,11)   (c,12)   (d,13)   (e,14)
     *   ----------------------------------------------------------------> t
     * }}}
     */
    def zipWithIndex(initialIndex: Int = 0): Stream[(A, Int)] =
      self.collect(initialIndex)((next, state) =>
        ((next, state), state + 1)
      )

    /**
     * @param initialTime the specified initial time. Defaults to 0.
     * @param clock a given [[Clock]] dictating the passage of time in the system.
     * @return a new [[Stream Stream]] containing pairs binding each event of
     *         this [[Stream Stream]] to a number indicating the instant in the
     *         continuous timeline when the event was received.
     *         The instant indicates the time elapsed since the creation of the
     *         [[Stream Stream]], which happened at the specified initial time.
     * @note see [[zipWithIndex]] for dealing with discrete time instead.
     * @example {{{
     *   s:                    | a           b           c           d           e
     *   s.zipWithTime():      | (a,10ns)    (b,20ns)    (c,45ns)    (d,60ns)    (e,70ns)
     *   s.zipWithTime(100ns): | (a,110ns)   (b,120ns)   (c,145ns)   (d,160ns)   (e,170ns)
     *   ------------------------10ns--------20ns--------45ns--------60ns--------70ns-----> t
     * }}}
     */
    def zipWithTime(initialTime: FiniteDuration = Duration.Zero)(
      using clock: Clock = Clock.SystemClock
    ): Stream[(A, FiniteDuration)] =
      self.collect(clock.time - initialTime)((next, state) => ((next, clock.time - state), state))

    /**
     * @param clock a given [[Clock]] dictating the passage of time in the system.
     * @return a new [[Stream Stream]] containing pairs binding each event of
     *         this [[Stream Stream]] to a number indicating the time elapsed
     *         in the continuous timeline since the previous event.
     * @example {{{
     *   s:               | a          b          c          d          e
     *   s.zipWithDelay:  | (a,10ns)   (b,10ns)   (c,25ns)   (d,15ns)   (e,10ns)
     *   -------------------10ns-------20ns-------45ns-------60ns-------70ns----> t
     * }}}
     */
    def zipWithDelay(using clock: Clock = Clock.SystemClock): Stream[(A, FiniteDuration)] =
      self.collect(clock.time)((next, state) =>
        val currentTime: FiniteDuration = clock.time
        ((next, currentTime - state), currentTime)
      )

    /**
     * @param n the specified number of events.
     * @return a new [[Stream Stream]] discarding the first events
     *         of this [[Stream Stream]], until the specified number
     *         of events have been discarded.
     * @example {{{
     *   s:         | a        b        c        d        e
     *   s.drop(0): | a        b        c        d        e
     *   s.drop(2): |                   c        d        e
     *   s.drop(5): |
     *   ---------------------------------------------------> t
     * }}}
     */
    def drop(n: Int): Stream[A] =
      self.zipWithIndex().filter(_._2 >= n).map(_._1)

    /**
     * Create a new [[Stream Stream]] which keeps track of the events
     * generated by this [[Stream Stream]].
     *
     * @param memory the specified memory (default: unlimited). The memory
     *               indicates the number of events the new [[Stream Stream]]
     *               will keep track of before discarding the oldest ones.
     * @return a new [[Stream Stream]] which keeps track of the events
     *         generated by this [[Stream Stream]].
     * @example {{{
     *   s:         | a     b       c         d           e
     *   s.cold():  | [a]   [a,b]   [a,b,c]   [a,b,c,d]   [a,b,c,d,e]
     *   s.cold(2): | [a]   [a,b]   [b,c]     [c,d]       [d,e]
     *   s.cold(3): | [a]   [a,b]   [a,b,c]   [b,c,d]     [c,d,e]
     *   ------------------------------------------------------------> t
     * }}}
     */
    def cold(memory: Int = Int.MaxValue): Stream[Seq[A]] =
      self.fold(Seq.empty[A])((acc, next) => (acc :+ next).takeRight(memory))

    /**
     * Group the events produced by this [[Stream Stream]] into groups of the
     * specified size.
     *
     * @param n the specified size.
     * @return a new [[Stream Stream]] obtained by grouping the events of this
     *         [[Stream Stream]] into groups of the specified size.
     * @note if not enough events are present in this [[Stream Stream]], then
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
    def ngramsOption(n: Int): Stream[Seq[Option[A]]] =
      self.fold(Seq.fill(n)(Option.empty[A]))((acc, next) => acc.drop(1) :+ Some(next))
    /** As [[ngramsOption ngramsOption(1)]]. */
    def unigramsOption: Stream[Option[A]] =
      self.ngramsOption(1).map(_.toTuple1)
    /** As [[ngramsOption ngramsOption(2)]]. */
    def bigramsOption: Stream[(Option[A], Option[A])] =
      self.ngramsOption(2).map(_.toTuple2)
    /** As [[ngramsOption ngramsOption(3)]]. */
    def trigramsOption: Stream[(Option[A], Option[A], Option[A])] =
      self.ngramsOption(3).map(_.toTuple3)

    /**
     * Group the events produced by this [[Stream Stream]] into groups of the
     * specified size.
     *
     * @param n the specified size.
     * @return a new [[Stream Stream]] obtained by grouping the events of this
     *         [[Stream Stream]] into groups of the specified size.
     * @note if not enough events are present in this [[Stream Stream]], then
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
    def ngrams(n: Int): Stream[Seq[A]] =
      self.ngramsOption(n).filter(_.forall(_.isDefined)).map(_.map(_.get))

    /** As [[ngrams ngrams(1)]]. */
    def unigrams: Stream[A] =
      self.ngrams(1).map(_.toTuple1)

    /** As [[ngrams ngrams(2)]]. */
    def bigrams: Stream[(A, A)] =
      self.ngrams(2).map(_.toTuple2)

    /** As [[ngrams ngrams(3)]]. */
    def trigrams: Stream[(A, A, A)] =
      self.ngrams(3).map(_.toTuple3)

    /**
     * Create a [[Stream Stream]] of the updates of this
     * [[Stream Stream]].
     *
     * @return a new [[Stream Stream]] obtained by discarding all
     *         consecutively repeated events in this [[Stream Stream]]
     *         (i.e. considering only the events that are new with respect to
     *         the latest event of [[Stream Stream]]).
     * @example {{{
     *   s:         | a   a   a   b   b   a   c   b   a   a   b
     *   s.updates: | a           b       a   c   b   a       b
     *   ------------------------------------------------------> t
     * }}}
     */
    def updates: Stream[A] =
      self
        .bigramsOption
        .filter((prev, next) => prev != next)
        .map(_._2)
        .defined

    /** Alias for [[updates]]. */
    def calm: Stream[A] = self.updates

    /**
     * As [[Stream.or Stream.or(this, other)]], but it produces a [[Stream Stream]]
     * of pairs containing the events of this [[Stream Stream]], the specified
     * [[Stream Stream]] or both of them in the case of simultaneous events.
     */
    def or[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
      Stream.untypedOr(self, other)
        .map(ab => (ab.get(0), ab.get(1)))
        .asInstanceOf[Stream[(Option[A], Option[B])]]

    /**
     * As [[Stream.and Stream.and(this, other)]], but it produces a [[Stream Stream]]
     * of pairs containing the simultaneous events of this [[Stream Stream]] and the
     * specified [[Stream Stream]].
     */
    def and[B](other: Stream[B]): Stream[(A, B)] =
      Stream.untypedAnd(self, other)
        .map(ab => (ab(0), ab(1)))
        .asInstanceOf[Stream[(A, B)]]

    /**
     * As [[Stream.sync Stream.sync(this, other)(memory)]], but it produces a
     * [[Stream Stream]] of pairs containing the synced events of this
     * [[Stream Stream]] and the specified [[Stream Stream]].
     */
    def sync[B](other: Stream[B], memory: Int = Int.MaxValue): Stream[(A, B)] =
      Stream.untypedSync(self, other)(memory)
        .map(ab => (ab(0), ab(1)))
        .asInstanceOf[Stream[(A, B)]]

    /**
     * Limit the rate at which this [[Stream Stream]] produces events
     * to the rate determined by the specified [[Stream Stream]], acting
     * as a throttler.
     *
     * @param throttler the specified throttler.
     * @tparam B the type of events produced by the specified throttler.
     * @return a new [[Stream Stream]] obtained by pairing the latest event of
     *         this [[Stream Stream]] with the latest event of the specified
     *         [[Stream Stream]], if they haven't been paired yet.
     * @note this is equivalent to [[sync sync(throttler, 1)]], therefore
     *       remembering only the latest event of both [[Stream Stream]]s.
     * @see if the events of the throttler aren't relevant, consider using
     *      [[throttle throttle(throttler)]].
     */
    def throttleWith[B](throttler: Stream[B]): Stream[(A, B)] =
      self.sync(throttler, memory = 1)

    /**
     * As [[throttleWith throttleWith(throttler)]], but it ignores the content
     * of the events generated by the throttler.
     */
    def throttle(throttler: Stream[?]): Stream[A] =
      self.throttleWith(throttler).map(_._1)
  }
