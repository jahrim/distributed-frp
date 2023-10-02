package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.StreamExtension.{*, given}
import it.unibo.distributedfrp.frp.StreamSample.*
import it.unibo.distributedfrp.utils.Symbols
import nz.sodium
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/** Test for [[StreamExtension]]. */
class StreamExtensionTest
  extends AnyFlatSpec
    with should.Matchers
    with Symbols
    with StreamSampleLoader:
  import scala.{None as N, Some as Y}

  private val Monitor = symbol("monitor")
  private val Defined = symbol("defined")
  private val CollectLazy = symbol("collectLazy")
  private val Fold = symbol("fold")
  private val ZipWithIndex = symbol("zipWithIndex")
  private val Cold = symbol("cold")
  private val NgramsOption = symbol("ngramsOption")
  private val UnigramsOption = symbol("unigramsOption")
  private val BigramsOption = symbol("bigramsOption")
  private val TrigramsOption = symbol("trigramsOption")
  private val Ngrams = symbol("ngrams")
  private val Unigrams = symbol("unigrams")
  private val Bigrams = symbol("bigrams")
  private val Trigrams = symbol("trigrams")
  private val Updates = symbol("updates")
  private val Calm = symbol("calm")
  private val Or = symbol("or")
  private val And = symbol("and")
  private val Sync = symbol("sync")
  private val WithThrottler = symbol("withThrottler")
  private val ThrottledBy = symbol("throttledBy")

  Monitor should "record all the events generated by a stream" in {
    val (sample, s) = load(intSample)
    val (_, monitor) = Stream.monitor(s)
    sample.generateEvents()
    monitor.eventLog shouldEqual Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }
  it should "discard the oldest recorded events after it has reached its maximum memory capacity" in {
    val (sample, s) = load(intSample)
    val (_, monitor) = Stream.monitor(s, memory = 3)
    sample.generateEvents()
    monitor.eventLog shouldEqual Seq(7, 8, 9)
  }

  Defined should "filter any empty options out of a stream" in {
    val (sample, s) = load(optionSample)
    val (_, definedMonitor) = Stream.monitor(s.defined)
    sample.generateEvents()
    definedMonitor.eventLog shouldEqual Seq(1, 2, 3)
  }

  CollectLazy should s"behave as the legacy `collectLazy`" in {
    val (sample, s) = load(intSample)
    val (_, collectMonitor) =
      Stream.monitor(s.collectLazy(0)((n, s) => { val ns = n + s; (ns, ns) }))
    val (_, legacyCollectMonitor) =
      Stream.monitor(s.collectLazy(sodium.Lazy(0), (n, s) => { val ns = n + s; sodium.Tuple2(ns, ns) }))
    sample.generateEvents()
    collectMonitor.eventLog shouldEqual Seq(0, 1, 3, 6, 10, 15, 21, 28, 36, 45)
    collectMonitor.eventLog shouldEqual legacyCollectMonitor.eventLog
  }

  Fold should "reduce the events of a stream as they are generated" in {
    val (sample, s) = load(intSample)
    val (_, foldMonitor) = Stream.monitor(s.fold(0)(_ + _))
    sample.generateEvents()
    foldMonitor.eventLog shouldEqual Seq(0, 1, 3, 6, 10, 15, 21, 28, 36, 45)
  }

  ZipWithIndex should "pair the events of a stream with the discrete time when they were received" in {
    val (sample, s) = load(stringSample)
    val (_, zipWithIndexMonitor) = Stream.monitor(s.zipWithIndex)
    sample.generateEvents()
    zipWithIndexMonitor.eventLog shouldEqual Seq(("a", 0), ("b", 1), ("c", 2), ("d", 3), ("e", 4))
  }

  Cold should "keep track of the events generated by a stream" in {
    val (sample, s) = load(stringSample)
    val (_, coldMonitorInf) = Stream.monitor(s.cold())
    val (_, coldMonitor2) = Stream.monitor(s.cold(memory = 2))
    val (_, coldMonitor3) = Stream.monitor(s.cold(memory = 3))
    sample.generateEvents()
    coldMonitorInf.eventLog shouldEqual Seq(
      "a" :: Nil,
      "a" :: "b" :: Nil,
      "a" :: "b" :: "c" :: Nil,
      "a" :: "b" :: "c" :: "d" :: Nil,
      "a" :: "b" :: "c" :: "d" :: "e" :: Nil,
    )
    coldMonitor2.eventLog shouldEqual Seq(
      "a" :: Nil,
      "a" :: "b" :: Nil,
      "b" :: "c" :: Nil,
      "c" :: "d" :: Nil,
      "d" :: "e" :: Nil,
    )
    coldMonitor3.eventLog shouldEqual Seq(
      "a" :: Nil,
      "a" :: "b" :: Nil,
      "a" :: "b" :: "c" :: Nil,
      "b" :: "c" :: "d" :: Nil,
      "c" :: "d" :: "e" :: Nil,
    )
  }

  NgramsOption should "produce all the n-grams in a stream without loss of events" in {
    val (sample, s) = load(stringSample)
    val (_, ngramsOptionMonitor1) = Stream.monitor(s.ngramsOption(n = 1))
    val (_, ngramsOptionMonitor2) = Stream.monitor(s.ngramsOption(n = 2))
    val (_, ngramsOptionMonitor3) = Stream.monitor(s.ngramsOption(n = 3))
    sample.generateEvents()
    ngramsOptionMonitor1.eventLog shouldEqual Seq(
      Y("a") :: Nil,
      Y("b") :: Nil,
      Y("c") :: Nil,
      Y("d") :: Nil,
      Y("e") :: Nil
    )
    ngramsOptionMonitor2.eventLog shouldEqual Seq(
      N      :: Y("a") :: Nil,
      Y("a") :: Y("b") :: Nil,
      Y("b") :: Y("c") :: Nil,
      Y("c") :: Y("d") :: Nil,
      Y("d") :: Y("e") :: Nil,
    )
    ngramsOptionMonitor3.eventLog shouldEqual Seq(
      N      :: N      :: Y("a") :: Nil,
      N      :: Y("a") :: Y("b") :: Nil,
      Y("a") :: Y("b") :: Y("c") :: Nil,
      Y("b") :: Y("c") :: Y("d") :: Nil,
      Y("c") :: Y("d") :: Y("e") :: Nil,
    )
  }
  it should "not lose the initial events if they cannot be grouped into an n-gram" in {
    val (sample, s) = load(stringSample)
    val (_, ngramsOptionMonitor6) = Stream.monitor(s.ngramsOption(n = 6))
    sample.generateEvents()
    ngramsOptionMonitor6.eventLog shouldBe Seq(
      N :: N      :: N      :: N      :: N      :: Y("a") :: Nil,
      N :: N      :: N      :: N      :: Y("a") :: Y("b") :: Nil,
      N :: N      :: N      :: Y("a") :: Y("b") :: Y("c") :: Nil,
      N :: N      :: Y("a") :: Y("b") :: Y("c") :: Y("d") :: Nil,
      N :: Y("a") :: Y("b") :: Y("c") :: Y("d") :: Y("e") :: Nil,
    )
  }

  UnigramsOption should "produce all the 1-grams in a stream without loss of initial events" in {
    val (sample, s) = load(stringSample)
    val (_, unigramsOptionMonitor) = Stream.monitor(s.unigramsOption)
    sample.generateEvents()
    unigramsOptionMonitor.eventLog shouldEqual Seq(
      Y("a"),
      Y("b"),
      Y("c"),
      Y("d"),
      Y("e")
    )
  }

  BigramsOption should "produce all the 2-grams in a stream without loss of initial events" in {
    val (sample, s) = load(stringSample)
    val (_, bigramsOptionMonitor) = Stream.monitor(s.bigramsOption)
    sample.generateEvents()
    bigramsOptionMonitor.eventLog shouldEqual Seq(
      (N,      Y("a")),
      (Y("a"), Y("b")),
      (Y("b"), Y("c")),
      (Y("c"), Y("d")),
      (Y("d"), Y("e")),
    )
  }
  it should "not lose the initial events if they cannot be grouped into an 2-gram" in {
    val (sample, s) = load(singletonSample)
    val (_, bigramsOptionMonitor) = Stream.monitor(s.bigramsOption)
    sample.generateEvents()
    bigramsOptionMonitor.eventLog shouldBe Seq((N, Y("a")))
  }

  TrigramsOption should "produce all the 3-grams in a stream without loss of initial events" in {
    val (sample, s) = load(stringSample)
    val (_, trigramsOptionMonitor) = Stream.monitor(s.trigramsOption)
    sample.generateEvents()
    trigramsOptionMonitor.eventLog shouldEqual Seq(
      (N,      N,      Y("a")),
      (N,      Y("a"), Y("b")),
      (Y("a"), Y("b"), Y("c")),
      (Y("b"), Y("c"), Y("d")),
      (Y("c"), Y("d"), Y("e")),
    )
  }
  it should "not lose the initial events if they cannot be grouped into an 3-gram" in {
    val (sample, s) = load(singletonSample)
    val (_, trigramsOptionMonitor) = Stream.monitor(s.trigramsOption)
    sample.generateEvents()
    trigramsOptionMonitor.eventLog shouldBe Seq((N, N, Y("a")))
  }

  Ngrams should "produce all the n-grams in a stream with loss of initial events" in {
    val (sample, s) = load(stringSample)
    val (_, ngramsMonitor1) = Stream.monitor(s.ngrams(n = 1))
    val (_, ngramsMonitor2) = Stream.monitor(s.ngrams(n = 2))
    val (_, ngramsMonitor3) = Stream.monitor(s.ngrams(n = 3))
    sample.generateEvents()
    ngramsMonitor1.eventLog shouldEqual Seq(
      "a" :: Nil,
      "b" :: Nil,
      "c" :: Nil,
      "d" :: Nil,
      "e" :: Nil
    )
    ngramsMonitor2.eventLog shouldEqual Seq(
      "a" :: "b" :: Nil,
      "b" :: "c" :: Nil,
      "c" :: "d" :: Nil,
      "d" :: "e" :: Nil,
    )
    ngramsMonitor3.eventLog shouldEqual Seq(
      "a" :: "b" :: "c" :: Nil,
      "b" :: "c" :: "d" :: Nil,
      "c" :: "d" :: "e" :: Nil,
    )
  }
  it should "lose the initial events if they cannot be grouped into an n-gram" in {
    val (sample, s) = load(stringSample)
    val (_, ngramsMonitor6) = Stream.monitor(s.ngrams(n = 6))
    sample.generateEvents()
    ngramsMonitor6.eventLog shouldBe empty
  }

  Unigrams should "produce all the 1-grams in a stream without loss of initial events" in {
    val (sample, s) = load(stringSample)
    val (_, unigramsMonitor) = Stream.monitor(s.unigrams)
    sample.generateEvents()
    unigramsMonitor.eventLog shouldEqual Seq("a", "b", "c", "d", "e")
  }

  Bigrams should "produce all the 2-grams in a stream without loss of initial events" in {
    val (sample, s) = load(stringSample)
    val (_, bigramsMonitor) = Stream.monitor(s.bigrams)
    sample.generateEvents()
    bigramsMonitor.eventLog shouldEqual Seq(
      ("a", "b"),
      ("b", "c"),
      ("c", "d"),
      ("d", "e"),
    )
  }
  it should "lose the initial events if they cannot be grouped into an 2-gram" in {
    val (sample, s) = load(singletonSample)
    val (_, bigramsMonitor) = Stream.monitor(s.bigrams)
    sample.generateEvents()
    bigramsMonitor.eventLog shouldBe empty
  }

  Trigrams should "produce all the 3-grams in a stream without loss of initial events" in {
    val (sample, s) = load(stringSample)
    val (_, trigramsMonitor) = Stream.monitor(s.trigrams)
    sample.generateEvents()
    trigramsMonitor.eventLog shouldEqual Seq(
      ("a", "b", "c"),
      ("b", "c", "d"),
      ("c", "d", "e"),
    )
  }
  it should "lose the initial events if they cannot be grouped into an 3-gram" in {
    val (sample, s) = load(singletonSample)
    val (_, trigramsMonitor) = Stream.monitor(s.trigrams)
    sample.generateEvents()
    trigramsMonitor.eventLog shouldBe empty
  }

  Updates should "filter out all the repeated consecutive events of a stream" in {
    val (sample, s) = load(repetitiveSample)
    val (_, updatesMonitor) = Stream.monitor(s.updates)
    sample.generateEvents()
    updatesMonitor.eventLog shouldBe Seq("a", "b", "a", "c", "b", "a", "b")
  }

  Calm should s"behave as $Updates" in {
    val (sample, s) = load(repetitiveSample)
    val (_, calmMonitor) = Stream.monitor(s.calm)
    val (_, updatesMonitor) = Stream.monitor(s.updates)
    sample.generateEvents()
    calmMonitor.eventLog shouldBe Seq("a", "b", "a", "c", "b", "a", "b")
    calmMonitor.eventLog shouldBe updatesMonitor.eventLog
  }

  Or should "merge all the events of two streams of different types" in {
    val (sample, (s1, s2)) = load(bistreamBitypedSample)
    val (_, orMonitor) = Stream.monitor(s1 or s2)
    sample.generateEvents()
    orMonitor.eventLog shouldEqual Seq(
      (Y(0), N),
      (Y(1), N),
      (Y(2), N),
      (N,    Y("a")),
      (N,    Y("b")),
      (N,    Y("c")),
      (Y(3), Y("d")),
      (Y(4), Y("e")),
    )
  }
  it should "merge all the events of many streams of the same type" in {
    val (sample, (s1, s2, s3)) = load(tristreamUnitypedSample)
    val (_, orMonitor) = Stream.monitor(Stream.or(s1, s2, s3))
    sample.generateEvents()
    orMonitor.eventLog shouldEqual Seq(
      Map(0 -> "a"),
      Map(0 -> "b"),
      Map(          1 -> "aa"),
      Map(          1 -> "bb", 2 -> "aaa"),
      Map(0 -> "c", 1 -> "cc", 2 -> "bbb"),
      Map(0 -> "d",            2 -> "ccc"),
    )
  }
  it should "merge all the events of many streams of different types" in {
    val (sample, (s1, s2, s3)) = load(tristreamTritypedSample)
    val (_, orMonitor) = Stream.monitor(Stream.untypedOr(s1, s2, s3))
    sample.generateEvents()
    orMonitor.eventLog shouldEqual Seq(
      Map(0 -> 0),
      Map(0 -> 1),
      Map(        1 -> "a"),
      Map(        1 -> "b", 2 -> true),
      Map(0 -> 2, 1 -> "c", 2 -> false),
      Map(0 -> 3,           2 -> true),
    )
  }

  And should "merge the simultaneous events of two streams of different types" in {
    val (sample, (s1, s2)) = load(bistreamBitypedSample)
    val (_, andMonitor) = Stream.monitor(s1 and s2)
    sample.generateEvents()
    andMonitor.eventLog shouldEqual Seq((3, "d"), (4, "e"))
  }
  it should "merge the simultaneous events of many streams of the same type" in {
    val (sample, (s1, s2, s3)) = load(tristreamUnitypedSample)
    val (_, andMonitor) = Stream.monitor(Stream.and(s1, s2, s3))
    sample.generateEvents()
    andMonitor.eventLog shouldEqual Seq(Map(0 -> "c", 1 -> "cc", 2 -> "bbb"))
  }
  it should "merge the simultaneous events of many streams of different types" in {
    val (sample, (s1, s2, s3)) = load(tristreamTritypedSample)
    val (_, andMonitor) = Stream.monitor(Stream.untypedAnd(s1, s2, s3))
    sample.generateEvents()
    andMonitor.eventLog shouldEqual Seq(Map(0 -> 2, 1 -> "c", 2 -> false))
  }

  Sync should "synchronize the events of two streams of different types" in {
    val (sample, (s1, s2)) = load(bistreamBitypedLongSample)
    val (_, syncMonitorInf) = Stream.monitor(s1 sync s2)
    val (_, syncMonitor1) = Stream.monitor(s1.sync(s2, memory = 1))
    val (_, syncMonitor2) = Stream.monitor(s1.sync(s2, memory = 2))
    sample.generateEvents()
    syncMonitorInf.eventLog shouldEqual Seq(
      (0, "a"),
      (1, "b"),
      (2, "c"),
      (3, "d"),
      (4, "e"),
      (5, "f")
    )
    syncMonitor1.eventLog shouldEqual Seq(
      (2, "a"),
      (3, "c"),
      (4, "e"),
      (5, "f")
    )
    syncMonitor2.eventLog shouldEqual Seq(
      (1, "a"),
      (2, "b"),
      (3, "c"),
      (4, "d"),
      (5, "e")
    )
  }
  it should "synchronize the events of many streams of the same type" in {
    val (sample, (s1, s2, s3)) = load(tristreamUnitypedLongSample)
    val (_, syncMonitorInf) = Stream.monitor(Stream.sync(s1, s2, s3)())
    val (_, syncMonitor1) = Stream.monitor(Stream.sync(s1, s2, s3)(memory = 1))
    val (_, syncMonitor2) = Stream.monitor(Stream.sync(s1, s2, s3)(memory = 2))
    sample.generateEvents()
    syncMonitorInf.eventLog shouldEqual Seq(
      Map(0 -> "a", 1 -> "aa", 2 -> "aaa"),
      Map(0 -> "b", 1 -> "bb", 2 -> "bbb"),
      Map(0 -> "c", 1 -> "cc", 2 -> "ccc"),
      Map(0 -> "d", 1 -> "dd", 2 -> "ddd"),
      Map(0 -> "e", 1 -> "ee", 2 -> "eee"),
      Map(0 -> "f", 1 -> "ff", 2 -> "fff"),
      Map(0 -> "g", 1 -> "gg", 2 -> "ggg"),
    )
    syncMonitor1.eventLog shouldEqual Seq(
      Map(0 -> "d", 1 -> "dd", 2 -> "aaa"),
      Map(0 -> "e", 1 -> "ff", 2 -> "eee"),
      Map(0 -> "g", 1 -> "gg", 2 -> "ggg"),
    )
    syncMonitor2.eventLog shouldEqual Seq(
      Map(0 -> "c", 1 -> "cc", 2 -> "aaa"),
      Map(0 -> "d", 1 -> "dd", 2 -> "bbb"),
      Map(0 -> "e", 1 -> "ee", 2 -> "ddd"),
      Map(0 -> "f", 1 -> "ff", 2 -> "eee"),
      Map(0 -> "g", 1 -> "gg", 2 -> "fff"),
    )
  }
  it should "synchronize the events of many streams of different types" in {
    val (sample, (s1, s2, s3)) = load(tristreamTritypedLongSample)
    val (_, syncMonitorInf) = Stream.monitor(Stream.untypedSync(s1, s2, s3)())
    val (_, syncMonitor1) = Stream.monitor(Stream.untypedSync(s1, s2, s3)(memory = 1))
    val (_, syncMonitor2) = Stream.monitor(Stream.untypedSync(s1, s2, s3)(memory = 2))
    sample.generateEvents()
    syncMonitorInf.eventLog shouldEqual Seq(
      Map(0 -> 0, 1 -> "a", 2 -> true),
      Map(0 -> 1, 1 -> "b", 2 -> false),
      Map(0 -> 2, 1 -> "c", 2 -> true),
      Map(0 -> 3, 1 -> "d", 2 -> false),
      Map(0 -> 4, 1 -> "e", 2 -> true),
      Map(0 -> 5, 1 -> "f", 2 -> false),
      Map(0 -> 6, 1 -> "g", 2 -> true),
    )
    syncMonitor1.eventLog shouldEqual Seq(
      Map(0 -> 3, 1 -> "d", 2 -> true),
      Map(0 -> 4, 1 -> "f", 2 -> true),
      Map(0 -> 6, 1 -> "g", 2 -> true),
    )
    syncMonitor2.eventLog shouldEqual Seq(
      Map(0 -> 2, 1 -> "c", 2 -> true),
      Map(0 -> 3, 1 -> "d", 2 -> false),
      Map(0 -> 4, 1 -> "e", 2 -> false),
      Map(0 -> 5, 1 -> "f", 2 -> true),
      Map(0 -> 6, 1 -> "g", 2 -> false),
    )
  }

  WithThrottler should "limit the event production rate (EPR) of a stream to the EPR of a throttler" in {
    val (sample, (s1, s2)) = load(bistreamBitypedLongSample)
    val (_, withThrottlerMonitor) = Stream.monitor(s1 withThrottler s2)
    sample.generateEvents()
    withThrottlerMonitor.eventLog shouldEqual Seq(
      (2, "a"),
      (3, "c"),
      (4, "e"),
      (5, "f")
    )
  }

  ThrottledBy should
    "limit the event production rate (EPR) of a stream to the EPR of a throttler, " +
    "ignoring the content of the events of the throttler" in {
    val (sample, (s1, s2)) = load(bistreamBitypedLongSample)
    val (_, throttledByMonitor) = Stream.monitor(s1 throttledBy s2)
    sample.generateEvents()
    throttledByMonitor.eventLog shouldEqual Seq(2, 3, 4, 5)
  }
