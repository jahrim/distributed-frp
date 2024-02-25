package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.StreamExtension.{*, given}
import it.unibo.distributedfrp.frp.StreamSample.*
import it.unibo.distributedfrp.test.utils.mock.clock.MockClockScheduler
import nz.sodium

import scala.concurrent.duration.*

/** Test for [[StreamExtension]]. */
class StreamExtensionTest extends StreamSampleTest:
  private val Monitor = symbol("monitor")
  private val Defined = symbol("defined")
  private val Collect = symbol("collect")
  private val Fold = symbol("fold")
  private val ZipWithIndex = symbol("zipWithIndex")
  private val ZipWithTime = symbol("zipWithTime")
  private val ZipWithDelay = symbol("zipWithDelay")
  private val Drop = symbol("drop")
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
  private val ThrottleWith = symbol("throttleWith")
  private val Throttle = symbol("throttle")

  import scala.{None as N, Some as Y}
  private given clock: MockClockScheduler = MockClockScheduler()

  Monitor should "record all the events generated by a stream" in {
    val (sample, s) = loadSample(intSample)
    val monitor = Stream.monitor(s)
    sample.generateEvents()
    monitor.eventLog shouldEqual Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }
  it should "discard the oldest recorded events after it has reached its maximum memory capacity" in {
    val (sample, s) = loadSample(intSample)
    val monitor = Stream.monitor(s, memory = 3)
    sample.generateEvents()
    monitor.eventLog shouldEqual Seq(7, 8, 9)
  }

  Defined should "filter any empty options out of a stream" in testSample(
    sample = optionSample,
    elaboration = _.defined,
    expectation = Seq(1, 2, 3),
  )

  Collect should s"behave as the legacy `collect`" in {
    val (sample, s) = loadSample(intSample)
    val collectMonitor =
      Stream.monitor(s.collect(0)((n, s) => { val ns = n + s; (ns, ns) }))
    val legacyCollectMonitor =
      Stream.monitor(s.collect(0, (n, s) => { val ns = n + s; sodium.Tuple2(ns, ns) }))
    sample.generateEvents()
    collectMonitor.eventLog shouldEqual Seq(0, 1, 3, 6, 10, 15, 21, 28, 36, 45)
    collectMonitor.eventLog shouldEqual legacyCollectMonitor.eventLog
  }

  Fold should "reduce the events of a stream as they are generated" in testSample(
    sample = intSample,
    elaboration = _.fold(0)(_ + _),
    expectation = Seq(0, 1, 3, 6, 10, 15, 21, 28, 36, 45),
  )

  ZipWithIndex should
    "pair the events of a stream with the instant in the discrete timeline " +
    "when they were received" in testSample(
    sample = stringSample,
    elaboration = _.zipWithIndex(),
    expectation = Seq(("a", 0), ("b", 1), ("c", 2), ("d", 3), ("e", 4)),
  )
  it should "shift the discrete timeline of the specified amount" in testSample(
    sample = stringSample,
    elaboration = _.zipWithIndex(10),
    expectation = Seq(("a", 10), ("b", 11), ("c", 12), ("d", 13), ("e", 14)),
  )

  ZipWithTime should
    "pair the events of a stream with the time in the continuous timeline " +
    "when they are received" in testSample(
    sample = timedIntSample,
    elaboration = _.zipWithTime(),
    expectation = Seq(
      0 -> 0.milliseconds,
      1 -> 150.milliseconds,
      2 -> 400.milliseconds,
      3 -> 450.milliseconds,
      4 -> 600.milliseconds,
    ),
  )
  it should "shift the continuous timeline of the specified amount" in testSample(
    sample = timedIntSample,
    elaboration = _.zipWithTime(10.seconds),
    expectation = Seq(
      0 -> 10_000.milliseconds,
      1 -> 10_150.milliseconds,
      2 -> 10_400.milliseconds,
      3 -> 10_450.milliseconds,
      4 -> 10_600.milliseconds,
    ),
  )

  ZipWithDelay should
    "pair the events of a stream with the time elapsed in the continuous timeline " +
    "since the previous events" in testSample(
    sample = timedIntSample,
    elaboration = _.zipWithDelay,
    expectation = Seq(
      0 -> 0.milliseconds,
      1 -> 150.milliseconds,
      2 -> 250.milliseconds,
      3 -> 50.milliseconds,
      4 -> 150.milliseconds,
    ),
  )

  Drop should "discard the first events generated by a stream until the specified amount" in testSample(
    sample = stringSample,
    elaboration = _.drop(2),
    expectation = Seq("c", "d", "e"),
  )
  it should "discard all elements if the specified amount exceeds the total number of events" in testSample(
    sample = stringSample,
    elaboration = _.drop(10),
    expectation = Seq(),
  )
  it should "discard no elements if the specified amount is 0" in testSample(
    sample = stringSample,
    elaboration = _.drop(0),
    expectation = Seq("a", "b", "c", "d", "e"),
  )

  Cold should "keep track of all the events generated by a stream" in testSample(
    sample = stringSample,
    elaboration = _.cold(),
    expectation = Seq(
      "a" :: Nil,
      "a" :: "b" :: Nil,
      "a" :: "b" :: "c" :: Nil,
      "a" :: "b" :: "c" :: "d" :: Nil,
      "a" :: "b" :: "c" :: "d" :: "e" :: Nil,
    ),
  )
  it should "limit the number of tracked events to the specified memory" in testSample(
    sample = stringSample,
    elaboration = _.cold(memory = 3),
    expectation = Seq(
      "a" :: Nil,
      "a" :: "b" :: Nil,
      "a" :: "b" :: "c" :: Nil,
      "b" :: "c" :: "d" :: Nil,
      "c" :: "d" :: "e" :: Nil,
    ),
  )

  NgramsOption should "produce all the n-grams in a stream without loss of events" in testSample(
    sample = stringSample,
    elaboration = _.ngramsOption(n = 4),
    expectation = Seq(
      N      :: N      :: N      :: Y("a") :: Nil,
      N      :: N      :: Y("a") :: Y("b") :: Nil,
      N      :: Y("a") :: Y("b") :: Y("c") :: Nil,
      Y("a") :: Y("b") :: Y("c") :: Y("d") :: Nil,
      Y("b") :: Y("c") :: Y("d") :: Y("e") :: Nil,
    ),
  )
  it should "produce all the unigrams in a stream without loss of events" in testSample(
    sample = stringSample,
    elaboration = _.ngramsOption(n = 1),
    expectation = Seq(
      Y("a") :: Nil,
      Y("b") :: Nil,
      Y("c") :: Nil,
      Y("d") :: Nil,
      Y("e") :: Nil
    ),
  )
  it should "not lose the initial events if they cannot be grouped into an n-gram" in testSample(
    sample = stringSample,
    elaboration = _.ngramsOption(n = 6),
    expectation = Seq(
      N :: N      :: N      :: N      ::      N :: Y("a") :: Nil,
      N :: N      :: N      :: N      :: Y("a") :: Y("b") :: Nil,
      N :: N      :: N      :: Y("a") :: Y("b") :: Y("c") :: Nil,
      N :: N      :: Y("a") :: Y("b") :: Y("c") :: Y("d") :: Nil,
      N :: Y("a") :: Y("b") :: Y("c") :: Y("d") :: Y("e") :: Nil,
    ),
  )

  UnigramsOption should "produce all the unigrams in a stream without loss of initial events" in testSample(
    sample = stringSample,
    elaboration = _.unigramsOption,
    expectation = Seq(
      Y("a"),
      Y("b"),
      Y("c"),
      Y("d"),
      Y("e")
    ),
  )

  BigramsOption should "produce all the bigrams in a stream without loss of initial events" in testSample(
    sample = stringSample,
    elaboration = _.bigramsOption,
    expectation = Seq(
      (N,      Y("a")),
      (Y("a"), Y("b")),
      (Y("b"), Y("c")),
      (Y("c"), Y("d")),
      (Y("d"), Y("e")),
    ),
  )
  it should "not lose the initial events if they cannot be grouped into an 2-gram" in testSample(
    sample = singletonSample,
    elaboration = _.bigramsOption,
    expectation = Seq((N, Y("a"))),
  )

  TrigramsOption should "produce all the 3-grams in a stream without loss of initial events" in testSample(
    sample = stringSample,
    elaboration = _.trigramsOption,
    expectation = Seq(
      (N,      N,      Y("a")),
      (N,      Y("a"), Y("b")),
      (Y("a"), Y("b"), Y("c")),
      (Y("b"), Y("c"), Y("d")),
      (Y("c"), Y("d"), Y("e")),
    ),
  )
  it should "not lose the initial events if they cannot be grouped into an 3-gram" in testSample(
    sample = singletonSample,
    elaboration = _.trigramsOption,
    expectation = Seq((N, N, Y("a"))),
  )

  Ngrams should "produce all the n-grams in a stream with loss of initial events" in testSample(
    sample = stringSample,
    elaboration = _.ngrams(n = 4),
    expectation = Seq(
      "a" :: "b" :: "c" :: "d" :: Nil,
      "b" :: "c" :: "d" :: "e" :: Nil,
    ),
  )
  it should "produce all the unigrams in a stream with loss of events" in testSample(
    sample = stringSample,
    elaboration = _.ngrams(n = 1),
    expectation = Seq(
      "a" :: Nil,
      "b" :: Nil,
      "c" :: Nil,
      "d" :: Nil,
      "e" :: Nil
    ),
  )
  it should "lose the initial events if they cannot be grouped into an n-gram" in testSample(
    sample = stringSample,
    elaboration = _.ngrams(n = 6),
    expectation = Seq.empty,
  )

  Unigrams should "produce all the unigrams in a stream with loss of initial events" in testSample(
    sample = stringSample,
    elaboration = _.unigrams,
    expectation = Seq("a", "b", "c", "d", "e"),
  )

  Bigrams should "produce all the bigrams in a stream with loss of initial events" in testSample(
    sample = stringSample,
    elaboration = _.bigrams,
    expectation = Seq(
      ("a", "b"),
      ("b", "c"),
      ("c", "d"),
      ("d", "e"),
    ),
  )
  it should "lose the initial events if they cannot be grouped into a bigram" in testSample(
    sample = singletonSample,
    elaboration = _.bigrams,
    expectation = Seq.empty,
  )

  Trigrams should "produce all the trigrams in a stream with loss of initial events" in testSample(
    sample = stringSample,
    elaboration = _.trigrams,
    expectation = Seq(
      ("a", "b", "c"),
      ("b", "c", "d"),
      ("c", "d", "e"),
    ),
  )
  it should "lose the initial events if they cannot be grouped into an trigram" in testSample(
    sample = singletonSample,
    elaboration = _.trigrams,
    expectation = Seq.empty,
  )

  Updates should "filter out all the repeated consecutive events of a stream" in testSample(
    sample = repetitiveSample,
    elaboration = _.updates,
    expectation = Seq("a", "b", "a", "c", "b", "a", "b"),
  )

  Calm should s"behave as $Updates" in {
    val (sample, s) = loadSample(repetitiveSample)
    val calmMonitor = Stream.monitor(s.calm)
    val updatesMonitor = Stream.monitor(s.updates)
    sample.generateEvents()
    calmMonitor.eventLog shouldBe Seq("a", "b", "a", "c", "b", "a", "b")
    calmMonitor.eventLog shouldBe updatesMonitor.eventLog
  }

  Or should "merge all the events of two streams of different types" in testSample(
    sample = bistreamBitypedSample,
    elaboration = (s1, s2) => s1 or s2,
    expectation = Seq(
      (Y(0), N),
      (Y(1), N),
      (Y(2), N),
      (N,    Y("a")),
      (N,    Y("b")),
      (N,    Y("c")),
      (Y(3), Y("d")),
      (Y(4), Y("e")),
    ),
  )
  it should "merge all the events of many streams of the same type" in testSample(
    sample = tristreamUnitypedSample,
    elaboration = (s1, s2, s3) => Stream.or(s1, s2, s3),
    expectation = Seq(
      Map(0 -> "a"),
      Map(0 -> "b"),
      Map(          1 -> "aa"),
      Map(          1 -> "bb", 2 -> "aaa"),
      Map(0 -> "c", 1 -> "cc", 2 -> "bbb"),
      Map(0 -> "d",            2 -> "ccc"),
    ),
  )
  it should "merge all the events of many streams of different types" in testSample(
    sample = tristreamTritypedSample,
    elaboration = (s1, s2, s3) => Stream.untypedOr(s1, s2, s3),
    expectation = Seq(
      Map(0 -> 0),
      Map(0 -> 1),
      Map(        1 -> "a"),
      Map(        1 -> "b", 2 -> true),
      Map(0 -> 2, 1 -> "c", 2 -> false),
      Map(0 -> 3,           2 -> true),
    ),
  )

  And should "merge the simultaneous events of two streams of different types" in testSample(
    sample = bistreamBitypedSample,
    elaboration = (s1, s2) => s1 and s2,
    expectation = Seq((3, "d"), (4, "e")),
  )
  it should "merge the simultaneous events of many streams of the same type" in testSample(
    sample = tristreamUnitypedSample,
    elaboration = (s1, s2, s3) => Stream.and(s1, s2, s3),
    expectation = Seq(Map(0 -> "c", 1 -> "cc", 2 -> "bbb")),
  )
  it should "merge the simultaneous events of many streams of different types" in testSample(
    sample = tristreamTritypedSample,
    elaboration = (s1, s2, s3) => Stream.untypedAnd(s1, s2, s3),
    expectation = Seq(Map(0 -> 2, 1 -> "c", 2 -> false)),
  )

  Sync should
    "synchronize the events of two streams of different types, " +
    "tracking all the events from all streams" in testSample(
    sample = bistreamBitypedLongSample,
    elaboration = (s1, s2) => s1 sync s2,
    expectation = Seq(
      (0, "a"),
      (1, "b"),
      (2, "c"),
      (3, "d"),
      (4, "e"),
      (5, "f")
    ),
  )
  it should
    "limit the number of tracked events for each of the " +
    "two streams to the specified memory" in testSample(
    sample = bistreamBitypedLongSample,
    elaboration = (s1, s2) => s1.sync(s2, memory = 2),
    expectation = Seq(
      (1, "a"),
      (2, "b"),
      (3, "c"),
      (4, "d"),
      (5, "e"),
    ),
  )
  it should
    "synchronize the events of many streams of the same type, " +
    "tracking all the events from all streams" in testSample(
    sample = tristreamUnitypedLongSample,
    elaboration = (s1, s2, s3) => Stream.sync(s1, s2, s3)(),
    expectation = Seq(
      Map(0 -> "a", 1 -> "aa", 2 -> "aaa"),
      Map(0 -> "b", 1 -> "bb", 2 -> "bbb"),
      Map(0 -> "c", 1 -> "cc", 2 -> "ccc"),
      Map(0 -> "d", 1 -> "dd", 2 -> "ddd"),
      Map(0 -> "e", 1 -> "ee", 2 -> "eee"),
      Map(0 -> "f", 1 -> "ff", 2 -> "fff"),
      Map(0 -> "g", 1 -> "gg", 2 -> "ggg"),
    ),
  )
  it should
    "limit the number of tracked events for each of the " +
    "streams of the same type to the specified memory" in testSample(
    sample = tristreamUnitypedLongSample,
    elaboration = (s1, s2, s3) => Stream.sync(s1, s2, s3)(memory = 2),
    expectation = Seq(
      Map(0 -> "c", 1 -> "cc", 2 -> "aaa"),
      Map(0 -> "d", 1 -> "dd", 2 -> "bbb"),
      Map(0 -> "e", 1 -> "ee", 2 -> "ddd"),
      Map(0 -> "f", 1 -> "ff", 2 -> "eee"),
      Map(0 -> "g", 1 -> "gg", 2 -> "fff"),
    ),
  )
  it should
    "synchronize the events of many streams of different types, " +
    "tracking all the events from all streams" in testSample(
    sample = tristreamTritypedLongSample,
    elaboration = (s1, s2, s3) => Stream.untypedSync(s1, s2, s3)(),
    expectation = Seq(
      Map(0 -> 0, 1 -> "a", 2 -> true),
      Map(0 -> 1, 1 -> "b", 2 -> false),
      Map(0 -> 2, 1 -> "c", 2 -> true),
      Map(0 -> 3, 1 -> "d", 2 -> false),
      Map(0 -> 4, 1 -> "e", 2 -> true),
      Map(0 -> 5, 1 -> "f", 2 -> false),
      Map(0 -> 6, 1 -> "g", 2 -> true),
    ),
  )
  it should
    "limit the number of tracked events for each of the " +
    "streams of different types to the specified memory" in testSample(
    sample = tristreamTritypedLongSample,
    elaboration = (s1, s2, s3) => Stream.untypedSync(s1, s2, s3)(memory = 2),
    expectation = Seq(
      Map(0 -> 2, 1 -> "c", 2 -> true),
      Map(0 -> 3, 1 -> "d", 2 -> false),
      Map(0 -> 4, 1 -> "e", 2 -> false),
      Map(0 -> 5, 1 -> "f", 2 -> true),
      Map(0 -> 6, 1 -> "g", 2 -> false),
    ),
  )

  ThrottleWith should "limit the event production rate (EPR) of a stream to the EPR of a throttler" in testSample(
    sample = bistreamBitypedLongSample,
    elaboration = (s1, s2) => s1 throttleWith s2,
    expectation = Seq(
      (2, "a"),
      (3, "c"),
      (4, "e"),
      (5, "f"),
    ),
  )

  Throttle should
    "limit the event production rate (EPR) of a stream to the EPR of a throttler, " +
    "ignoring the content of the events of the throttler" in testSample(
    sample = bistreamBitypedLongSample,
    elaboration = (s1, s2) => s1 throttle s2,
    expectation = Seq(2, 3, 4, 5),
  )
