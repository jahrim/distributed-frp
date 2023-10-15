package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.FiniteStreamExtension.{*, given}
import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.frp.StreamSample.*
import it.unibo.distributedfrp.frp.timer.TimerFactory
import it.unibo.distributedfrp.utils.Symbols
import it.unibo.distributedfrp.utils.mock.{MockClockScheduler, MockTimerFactory}
import nz.sodium
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.concurrent.duration.*

/** Test for [[FiniteStream]]. */
class FiniteStreamExtensionTest
  extends AnyFlatSpec
    with should.Matchers
    with BeforeAndAfter
    with Symbols
    with StreamSampleLoader:
  import FiniteStreamExtension.FiniteEvent.{EOS, Event as E}

  private val Finite = symbol("finite")
  private val MapPayload = symbol("mapPayload")
  private val FilterPayload = symbol("filterPayload")
  private val Until = symbol("until")
  private val Take = symbol("take")
  private val TakeBefore = symbol("takeBefore")
  private val TakeBeforeInactivityOf = symbol("takeBeforeInactivityOf")
  private val InterruptBy = symbol("interruptBy")
  private val InterruptAfter = symbol("interruptAfter")
  private val InterruptAfterInactivityOf = symbol("interruptAfterInactivityOf")

  given clock: MockClockScheduler = MockClockScheduler()
  given timerFactory: TimerFactory[?] = MockTimerFactory.basic

  Finite should "map a stream into the corresponding finite stream" in {
    val (sample, s) = load(stringSample)
    val (_, mapPayloadMonitor) = Stream.monitor(s.finite)
    sample.generateEvents()
    mapPayloadMonitor.eventLog shouldEqual Seq(E("a"), E("b"), E("c"), E("d"), E("e"))
  }

  MapPayload should "map the events of a finite stream, leaving EOS untouched" in {
    val (sample, s) = load(finiteStreamSample)
    val (_, mapPayloadMonitor) = Stream.monitor(s.mapPayload("a".repeat))
    sample.generateEvents()
    mapPayloadMonitor.eventLog shouldEqual Seq(E(""), E("a"), E("aa"), E("aaa"), EOS)
  }

  FilterPayload should "filter the events of a finite stream, leaving EOS untouched" in {
    val (sample, s) = load(finiteStreamSample)
    val (_, filterPayloadMonitor) = Stream.monitor(s.filterPayload(_ > 1))
    sample.generateEvents()
    filterPayloadMonitor.eventLog shouldEqual Seq(E(2), E(3), EOS)
  }

  Until should "end a finite stream when a predicate holds for a given event" in {
    val (sample, s) = load(finiteStreamSample)
    val (_, untilMonitor) = Stream.monitor(s.until(_ > 1))
    sample.generateEvents()
    untilMonitor.eventLog shouldEqual Seq(E(0), E(1), EOS)
  }
  it should "end a finite stream if the stream ends on its own" in {
    val (sample, s) = load(finiteStreamSample)
    val (_, untilMonitor) = Stream.monitor(s.until(_ > 5))
    sample.generateEvents()
    untilMonitor.eventLog shouldEqual Seq(E(0), E(1), E(2), E(3), EOS)
  }
  it should "not end a finite stream if the predicate is not defined for a given event" in {
    val (sample, s) = load(finiteStreamSample)
    val (_, untilMonitor) = Stream.monitor(s.until({ case 2 => true }))
    sample.generateEvents()
    untilMonitor.eventLog shouldEqual Seq(E(0), E(1), EOS)
  }
  it should s"combine correctly with other $Until operators" in {
    val (sample, s) = load(finiteStreamSample)
    val (_, untilMonitor) = Stream.monitor(s.until(_ > 2).until(_ > 1))
    sample.generateEvents()
    untilMonitor.eventLog shouldEqual Seq(E(0), E(1), EOS)
  }

  Take should "end a finite stream when it fires an event after a given number of events" in {
    val (sample, s) = load(finiteStreamSample)
    val (_, takeMonitor) = Stream.monitor(s.take(2))
    sample.generateEvents()
    takeMonitor.eventLog shouldEqual Seq(E(0), E(1), EOS)
  }
  it should "end a finite stream if the stream ends on its own" in {
    val (sample, s) = load(finiteStreamSample)
    val (_, takeMonitor) = Stream.monitor(s.take(10))
    sample.generateEvents()
    takeMonitor.eventLog shouldEqual Seq(E(0), E(1), E(2), E(3), EOS)
  }
  it should s"combine correctly with other $Take operators" in {
    val (sample, s) = load(finiteStreamSample)
    val (_, takeMonitor) = Stream.monitor(s.take(10).take(2))
    sample.generateEvents()
    takeMonitor.eventLog shouldEqual Seq(E(0), E(1), EOS)
  }

  TakeBefore should
    "end a finite stream when an event is fired after a" +
    "certain duration since its call" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, takeBeforeMonitor) = Stream.monitor(s.takeBefore(200.milliseconds))
    sample.generateEvents()
    takeBeforeMonitor.eventLog shouldEqual Seq(E(0), E(1), EOS)
  }
  it should "end a finite stream if the stream ends on its own" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, takeBeforeMonitor) = Stream.monitor(s.takeBefore(800.milliseconds))
    sample.generateEvents()
    takeBeforeMonitor.eventLog shouldEqual Seq(E(0), E(1), E(2), E(3), EOS)
  }
  it should "not end a finite stream if the stream does not fire any event after the specified duration" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, neverEndingTakeBeforeMonitor) = Stream.monitor(s.filter(_ != EOS).takeBefore(800.milliseconds))
    sample.generateEvents()
    neverEndingTakeBeforeMonitor.eventLog shouldEqual Seq(E(0), E(1), E(2), E(3))
  }
  it should s"combine correctly with other $TakeBefore operators" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, takeBeforeMonitor) = Stream.monitor(s.takeBefore(800.milliseconds).takeBefore(200.milliseconds))
    sample.generateEvents()
    takeBeforeMonitor.eventLog shouldEqual Seq(E(0), E(1), EOS)
  }

  TakeBeforeInactivityOf should
    "end a stream when an event is fired after a " +
    "certain duration since the previous event" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, takeBeforeInactivityOfMonitor) = Stream.monitor(s.takeBeforeInactivityOf(200.milliseconds))
    sample.generateEvents()
    takeBeforeInactivityOfMonitor.eventLog shouldEqual Seq(E(0), E(1), E(2), EOS)
  }
  it should "end a finite stream if the stream ends on its own" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, takeBeforeInactivityOfMonitor) = Stream.monitor(s.takeBeforeInactivityOf(400.milliseconds))
    sample.generateEvents()
    takeBeforeInactivityOfMonitor.eventLog shouldEqual Seq(E(0), E(1), E(2), E(3), EOS)
  }
  it should "not end a finite stream if the stream does not fire any event after the specified duration" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, takeBeforeInactivityOfMonitor) =
      Stream.monitor(s.filter(_ != EOS).takeBeforeInactivityOf(400.milliseconds))
    sample.generateEvents()
    takeBeforeInactivityOfMonitor.eventLog shouldEqual Seq(E(0), E(1), E(2), E(3))
  }
  it should s"combine correctly with other $TakeBeforeInactivityOf operators" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, takeBeforeInactivityOfMonitor) =
      Stream.monitor(s.takeBeforeInactivityOf(400.milliseconds).takeBeforeInactivityOf(200.milliseconds))
    sample.generateEvents()
    takeBeforeInactivityOfMonitor.eventLog shouldEqual Seq(E(0), E(1), E(2), EOS)
  }

  InterruptBy should "end a finite stream when another stream fires an event" in {
    val (sample, (s1, s2, _)) = load(finiteTristreamSample)
    val (_, interruptByMonitor) = Stream.monitor(s1.interruptBy(s2))
    sample.generateEvents()
    interruptByMonitor.eventLog shouldEqual Seq(E(0), E(1), EOS)
  }
  it should "end a finite stream if the stream ends on its own" in {
    val (sample, (s1, _, s3)) = load(finiteTristreamSample)
    val (_, interruptByMonitor) = Stream.monitor(s1.interruptBy(s3))
    sample.generateEvents()
    interruptByMonitor.eventLog shouldEqual Seq(E(0), E(1), E(2), E(3), EOS)
  }
  it should s"combine correctly with other $InterruptBy operators" in {
    val (sample, (s1, s2, s3)) = load(finiteTristreamSample)
    val (_, interruptByMonitor) = Stream.monitor(s1.interruptBy(s3).interruptBy(s2))
    sample.generateEvents()
    interruptByMonitor.eventLog shouldEqual Seq(E(0), E(1), EOS)
  }

  InterruptAfter should "end a finite stream after a certain duration since its call" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, interruptAfterMonitor) = Stream.monitor(s.interruptAfter(200.milliseconds).zipWithTime())
    sample.generateEvents()
    interruptAfterMonitor.eventLog shouldEqual Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      EOS  -> 200.milliseconds,
    )
  }
  it should "end a finite stream if the stream ends on its own" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, interruptAfterMonitor) = Stream.monitor(s.interruptAfter(800.milliseconds).zipWithTime())
    sample.generateEvents()
    interruptAfterMonitor.eventLog shouldEqual Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      E(3) -> 500.milliseconds,
      EOS  -> 600.milliseconds,
    )
  }
  it should "end a finite stream even if the stream does not fire any event after the specified duration" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, interruptAfterMonitor) =
      Stream.monitor(s.filter(_ != EOS).interruptAfter(800.milliseconds).zipWithTime())
    sample.generateEvents()
    interruptAfterMonitor.eventLog shouldEqual Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      E(3) -> 500.milliseconds,
      EOS  -> 800.milliseconds,
    )
  }
  it should s"combine correctly with other $InterruptAfter operators" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, interruptAfterMonitor) =
      Stream.monitor(s.interruptAfter(800.milliseconds).interruptAfter(200.milliseconds).zipWithTime())
    sample.generateEvents()
    interruptAfterMonitor.eventLog shouldEqual Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      EOS  -> 200.milliseconds,
    )
  }

  InterruptAfterInactivityOf should "end a finite stream after a certain duration since its previous event" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, interruptAfterInactivityOfMonitor) =
      Stream.monitor(s.interruptAfterInactivityOf(200.milliseconds).zipWithTime())
    sample.generateEvents()
    interruptAfterInactivityOfMonitor.eventLog shouldEqual Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      EOS  -> 450.milliseconds,
    )
  }
  it should "end a finite stream if the stream ends on its own" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, interruptAfterInactivityOfMonitor) =
      Stream.monitor(s.interruptAfterInactivityOf(800.milliseconds).zipWithTime())
    sample.generateEvents()
    interruptAfterInactivityOfMonitor.eventLog shouldEqual Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      E(3) -> 500.milliseconds,
      EOS  -> 600.milliseconds,
    )
  }
  it should "end a finite stream even if the stream does not fire any event after the specified duration" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, interruptAfterInactivityOfMonitor) =
      Stream.monitor(s.filter(_ != EOS).interruptAfterInactivityOf(800.milliseconds).zipWithTime())
    sample.generateEvents()
    interruptAfterInactivityOfMonitor.eventLog shouldEqual Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      E(3) -> 500.milliseconds,
      EOS  -> 1300.milliseconds,
    )
  }
  it should s"combine correctly with other $InterruptAfterInactivityOf operators" in {
    val (sample, s) = load(timedFiniteStreamSample)
    val (_, interruptAfterInactivityOfMonitor) =
      Stream.monitor(s.interruptAfterInactivityOf(800.milliseconds).interruptAfterInactivityOf(200.milliseconds).zipWithTime())
    sample.generateEvents()
    interruptAfterInactivityOfMonitor.eventLog shouldEqual Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      EOS  -> 450.milliseconds,
    )
  }
