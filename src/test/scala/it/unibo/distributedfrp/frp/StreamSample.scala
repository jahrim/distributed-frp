package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.StreamSinkExtension.*
import nz.sodium

/**
 * A sample of pre-configured [[sodium.Stream Stream]]s.
 *
 * @tparam SS the type of collection containing the pre-configured
 *            [[sodium.Stream Stream]]s.
 */
trait StreamSample[SS](val streams: SS):
  /**
   * Start generating the events of the pre-configured
   * [[sodium.Stream Stream]]s of this [[StreamSample]].
   */
  def generateEvents(): Unit

/** Companion object of [[StreamSample]]. */
object StreamSample:
  import sodium.StreamSink as Sink

  /**
   * @param events the specified events.
   * @return a new [[StreamSample]] of a single [[sodium.Stream Stream]]
   *         generating the specified events.
   */
  def singleStreamSample[A](events: A*): StreamSample[Sink[A]] =
    new StreamSample(Sink[A]()):
      private val s = streams
      override def generateEvents(): Unit = s.!(events*)

  /** {{{ s: | None Some(1) Some(2) None None Some(3) }}} */
  def optionSample: StreamSample[Sink[Option[Int]]] =
    singleStreamSample(Seq(None, Some(1), Some(2), None, None, Some(3)) *)

  /** {{{ s: | 0 1 2 3 4 5 6 7 8 9 }}} */
  def intSample: StreamSample[Sink[Int]] =
    singleStreamSample(Seq.range(0, 10) *)

  /** {{{ s: | a b c d e }}} */
  def stringSample: StreamSample[Sink[String]] =
    singleStreamSample(Seq.range('a', 'f').map(_.toString) *)

  /** {{{ s: | a }}} */
  def singletonSample: StreamSample[Sink[String]] =
    singleStreamSample("a")

  /** {{{ s: | a a a b b a c b a a b }}} */
  def repetitiveSample: StreamSample[Sink[String]] =
    singleStreamSample("a", "a", "a", "b", "b", "a", "c", "b", "a", "a", "b")

  /**
   * {{{
   * s1: | 0 1 2       3 4
   * s2: |       a b c d e
   * }}}
   */
  def bistreamBitypedSample: StreamSample[(Sink[Int], Sink[String])] =
    new StreamSample((Sink[Int](), Sink[String]())):
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
   * }}}
   */
  def tristreamUnitypedSample: StreamSample[(Sink[String], Sink[String], Sink[String])] =
    new StreamSample((Sink[String](), Sink[String](), Sink[String]())):
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
   * }}}
   */
  def tristreamTritypedSample: StreamSample[(Sink[Int], Sink[String], Sink[Boolean])] =
    new StreamSample((Sink[Int](), Sink[String](), Sink[Boolean]())):
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
   * }}}
   */
  def bistreamBitypedLongSample: StreamSample[(Sink[Int], Sink[String])] =
    new StreamSample((Sink[Int](), Sink[String]())):
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
   * }}}
   */
  def tristreamUnitypedLongSample: StreamSample[(Sink[String], Sink[String], Sink[String])] =
    new StreamSample((Sink[String](), Sink[String](), Sink[String]())):
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
   * }}}
   */
  def tristreamTritypedLongSample: StreamSample[(Sink[Int], Sink[String], Sink[Boolean])] =
    new StreamSample((Sink[Int](), Sink[String](), Sink[Boolean]())):
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
