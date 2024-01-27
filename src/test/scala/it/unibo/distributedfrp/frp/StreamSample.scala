package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.FiniteStreamExtension.*
import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.frp.StreamSinkExtension.*
import it.unibo.distributedfrp.test.utils.mock.clock.MockClockScheduler

import scala.concurrent.duration.*

/**
 * A sample of pre-configured [[Stream Stream]]s.
 *
 * @tparam SS the type of collection containing the pre-configured
 *            [[Stream Stream]]s (maybe a single [[Stream Stream]]).
 */
trait StreamSample[+SS](val streams: SS):
  /**
   * Start generating the events of the pre-configured
   * [[Stream Stream]]s of this [[StreamSample]].
   */
  def generateEvents(): Unit

/** Companion object of [[StreamSample]]. */
object StreamSample:
  /**
   * @param events the specified events.
   * @return a new [[StreamSample]] of a single [[Stream Stream]]
   *         generating the specified events.
   */
  def singleStreamSample[A](events: A*): StreamSample[Stream[A]] =
    new StreamSample(StreamSink[A]()):
      private val s = streams
      override def generateEvents(): Unit = s.!(events*)

  /**
   * @param events    the specified events bound to the specified times.
   * @param scheduler the [[MockClockScheduler]] used to schedule the
   *                  generation of the events.
   * @return a new [[StreamSample]] of a single [[Stream Stream]]
   *         generating the specified events at the specified times, using
   *         the given [[MockClockScheduler]].
   * @note [[StreamSample.generateEvents generateEvents()]] will execute all
   *       the pending tasks of the [[MockClockScheduler]].
   */
  def timedSingleStreamSample[A](events: (A, FiniteDuration)*)(
    using scheduler: MockClockScheduler
  ): StreamSample[Stream[A]] =
    new StreamSample(StreamSink[A]()):
      private val s = streams
      events.foreach((e, t) => scheduler.scheduleAt(t){ s ! e })
      override def generateEvents(): Unit = scheduler.executePending()

  /** {{{ s: | None Some(1) Some(2) None None Some(3) }}} */
  def optionSample: StreamSample[Stream[Option[Int]]] =
    singleStreamSample(Seq(None, Some(1), Some(2), None, None, Some(3)) *)

  /** {{{ s: | 0 1 2 3 4 5 6 7 8 9 }}} */
  def intSample: StreamSample[Stream[Int]] =
    singleStreamSample(Seq.range(0, 10) *)

  /**
   * {{{
   * s: | 0      1        2        3        4
   * -----0ms----150ms----400ms----450ms----600ms-> t
   * }}}
   * @see [[timedSingleStreamSample]] for more information.
   */
  def timedIntSample(using MockClockScheduler): StreamSample[Stream[Int]] =
    timedSingleStreamSample(
      0 -> 0.milliseconds,
      1 -> 150.milliseconds,
      2 -> 400.milliseconds,
      3 -> 450.milliseconds,
      4 -> 600.milliseconds,
    )

  /** {{{ s: | a b c d e }}} */
  def stringSample: StreamSample[Stream[String]] =
    singleStreamSample(Seq.range('a', 'f').map(_.toString) *)

  /** {{{ s: | a }}} */
  def singletonSample: StreamSample[Stream[String]] =
    singleStreamSample("a")

  /** {{{ s: | a a a b b a c b a a b }}} */
  def repetitiveSample: StreamSample[Stream[String]] =
    singleStreamSample("a", "a", "a", "b", "b", "a", "c", "b", "a", "a", "b")

  /**
   * {{{
   * s1: | 0 1 2       3 4
   * s2: |       a b c d e
   * ----------------------> t
   * }}}
   */
  def bistreamBitypedSample: StreamSample[(Stream[Int], Stream[String])] =
    new StreamSample((StreamSink[Int](), StreamSink[String]())):
      private val (s1, s2) = streams
      override def generateEvents(): Unit =
        s1 ! (0, 1, 2)
        s2 ! ("a", "b", "c")
        (s1, s2) ! ((3, "d"), (4, "e"))

  /**
   * {{{
   * s1: | a b        c   d
   * s2: |     aa bb  cc
   * s3: |        aaa bbb ccc
   * -------------------------> t
   * }}}
   */
  def tristreamUnitypedSample: StreamSample[(Stream[String], Stream[String], Stream[String])] =
    new StreamSample((StreamSink[String](), StreamSink[String](), StreamSink[String]())):
      private val (s1, s2, s3) = streams
      override def generateEvents(): Unit =
        s1 ! ("a", "b")
        s2 ! ("aa")
        (s2, s3) ! (("bb", "aaa"))
        (s1, s2, s3) ! (("c", "cc", "bbb"))
        (s1, s3) ! (("d", "ccc"))

  /**
   * {{{
   * s1: | 0 1        2     3
   * s2: |     a b    c
   * s3: |       true false true
   * ----------------------------> t
   * }}}
   */
  def tristreamTritypedSample: StreamSample[(Stream[Int], Stream[String], Stream[Boolean])] =
    new StreamSample((StreamSink[Int](), StreamSink[String](), StreamSink[Boolean]())):
      private val (s1, s2, s3) = streams
      override def generateEvents(): Unit =
        s1 ! (0, 1)
        s2 ! ("a")
        (s2, s3) ! (("b", true))
        (s1, s2, s3) ! ((2, "c", false))
        (s1, s3) ! ((3, true))

  /**
   * {{{
   * s1: | 0 1 2     3     4 5
   * s2: |       a b c d e   f
   * --------------------------> t
   * }}}
   */
  def bistreamBitypedLongSample: StreamSample[(Stream[Int], Stream[String])] =
    new StreamSample((StreamSink[Int](), StreamSink[String]())):
      private val (s1, s2) = streams
      override def generateEvents(): Unit =
        s1 ! (0, 1, 2)
        s2 ! ("a", "b")
        (s1, s2) ! ((3, "c"))
        s2 ! ("d", "e")
        s1 ! (4)
        (s1, s2) ! ((5, "f"))

  /**
   * {{{
   * s1: | a b c          d                         e f   g
   * s2: |       aa bb cc dd                 ee  ff       gg
   * s3: |                   aaa bbb ccc ddd eee      fff ggg
   * ---------------------------------------------------------> t
   * }}}
   */
  def tristreamUnitypedLongSample: StreamSample[(Stream[String], Stream[String], Stream[String])] =
    new StreamSample((StreamSink[String](), StreamSink[String](), StreamSink[String]())):
      private val (s1, s2, s3) = streams
      override def generateEvents(): Unit =
        s1 ! ("a", "b", "c")
        s2 ! ("aa", "bb", "cc")
        (s1, s2) ! (("d", "dd"))
        s3 ! ("aaa", "bbb", "ccc", "ddd")
        (s2, s3) ! (("ee", "eee"))
        s2 ! ("ff")
        s1 ! ("e")
        (s1, s3) ! (("f", "fff"))
        (s1, s2, s3) ! (("g", "gg", "ggg"))

  /**
   * {{{
   * s1: | 0 1 2       3                              4 5     6
   * s2: |       a b c d                       e    f         g
   * s3: |               true false true false true     false true
   * --------------------------------------------------------------> t
   * }}}
   */
  def tristreamTritypedLongSample: StreamSample[(Stream[Int], Stream[String], Stream[Boolean])] =
    new StreamSample((StreamSink[Int](), StreamSink[String](), StreamSink[Boolean]())):
      private val (s1, s2, s3) = streams
      override def generateEvents(): Unit =
        s1 ! (0, 1, 2)
        s2 ! ("a", "b", "c")
        (s1, s2) ! ((3, "d"))
        s3 ! (true, false, true, false)
        (s2, s3) ! (("e", true))
        s2 ! ("f")
        s1 ! (4)
        (s1, s3) ! ((5, false))
        (s1, s2, s3) ! ((6, "g", true))

  import FiniteStreamExtension.FiniteEvent.{EOS, Event as E}

  /** {{{ s: | E(0) E(1) E(2) E(3) EOS }}} */
  def finiteStreamSample: StreamSample[FiniteStream[Int]] =
    singleStreamSample(E(0), E(1), E(2), E(3), EOS)

  /** {{{ s: | E(0) E(1) E(2) E(3) }}} */
  def infiniteFiniteStreamSample: StreamSample[FiniteStream[Int]] =
    singleStreamSample(E(0), E(1), E(2), E(3))

  /** {{{ s: | EOS }}} */
  def emptyFiniteStreamSample[S]: StreamSample[FiniteStream[S]] =
    singleStreamSample(EOS)

  /**
   * {{{
   * s1: | E(0) E(1) E(2) E(3) EOS
   * s2: |           a    b
   * s3: |                         0
   * --------------------------------> t
   * }}}
   */
  def finiteTristreamSample: StreamSample[(FiniteStream[Int], Stream[String], Stream[Int])] =
    new StreamSample((StreamSink[FiniteEvent[Int]](), StreamSink[String](), StreamSink[Int]())):
      private val (s1, s2, s3) = streams
      override def generateEvents(): Unit =
        s1 ! (E(0), E(1))
        (s1, s2) ! ((E(2), "a"), (E(3), "b"))
        s1 ! EOS
        s3 ! 0

  /**
   * {{{
   * s: | E(0)   E(1)     E(2)     E(3)     EOS
   * -----0ms----150ms----250ms----500ms----600ms-> t
   * }}}
   * @see [[timedSingleStreamSample]] for more information.
   */
  def timedFiniteStreamSample(using MockClockScheduler): StreamSample[FiniteStream[Int]] =
    timedSingleStreamSample(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      E(3) -> 500.milliseconds,
      EOS  -> 600.milliseconds,
    )
