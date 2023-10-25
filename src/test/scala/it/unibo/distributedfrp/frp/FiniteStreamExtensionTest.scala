package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.FiniteStreamExtension.{*, given}
import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.frp.StreamSample.*
import it.unibo.distributedfrp.frp.timer.TimerFactory
import it.unibo.distributedfrp.test.utils.mock.clock.MockClockScheduler
import it.unibo.distributedfrp.test.utils.mock.timer.MockTimerFactory

import scala.concurrent.Future
import scala.concurrent.duration.*

/** Test for [[FiniteStream]]. */
class FiniteStreamExtensionTest extends StreamSampleTest:
  private val Finite = symbol("finite")
  private val Events = symbol("events")
  private val Eos = symbol("eos")
  private val Termination = symbol("Termination")
  private val MapPayload = symbol("mapPayload")
  private val FilterPayload = symbol("filterPayload")
  private val Until = symbol("until")
  private val Take = symbol("take")
  private val TakeBefore = symbol("takeBefore")
  private val TakeBeforeInactivityOf = symbol("takeBeforeInactivityOf")
  private val InterruptBy = symbol("interruptBy")
  private val InterruptAfter = symbol("interruptAfter")
  private val InterruptAfterInactivityOf = symbol("interruptAfterInactivityOf")

  import FiniteStreamExtension.FiniteEvent.{EOS, Event as E}
  private given clock: MockClockScheduler = MockClockScheduler()
  private given timerFactory: TimerFactory[?] = MockTimerFactory.basic

  Finite should "map a stream into the corresponding finite stream" in testSample(
    sample = stringSample,
    elaboration = _.finite,
    expectation = Seq(E("a"), E("b"), E("c"), E("d"), E("e")),
  )

  Events should "keep all the events with payload of a finite stream, discarding the EOS event" in testSample(
    sample = finiteStreamSample,
    elaboration = _.events,
    expectation = Seq(E(0), E(1), E(2), E(3)),
  )

  Eos should "keep the EOS event of a finite stream, discarding all the events with payload" in testSample(
    sample = finiteStreamSample,
    elaboration = _.eos,
    expectation = Seq(EOS),
  )

  Termination should "complete when the finite stream fires its next EOS event" in {
    val (sample, s) = loadSample(finiteStreamSample)
    val streamTermination: Future[Unit] = s.termination
    streamTermination.isCompleted shouldBe false
    sample.generateEvents()
    streamTermination.isCompleted shouldBe true
  }

  MapPayload should "map the events of a finite stream, leaving EOS untouched" in testSample(
    sample = finiteStreamSample,
    elaboration = _.mapPayload("a".repeat),
    expectation = Seq(E(""), E("a"), E("aa"), E("aaa"), EOS),
  )

  FilterPayload should "filter the events of a finite stream, leaving EOS untouched" in testSample(
    sample = finiteStreamSample,
    elaboration = _.filterPayload(_ > 1),
    expectation = Seq(E(2), E(3), EOS),
  )

  Until should "end a finite stream when a predicate holds for a given event" in testSample(
    sample = finiteStreamSample,
    elaboration = _.until(_ > 1),
    expectation = Seq(E(0), E(1), EOS),
  )
  it should "end a finite stream if the stream ends on its own" in testSample(
    sample = finiteStreamSample,
    elaboration = _.until(_ > 5),
    expectation = Seq(E(0), E(1), E(2), E(3), EOS),
  )
  it should "not end a finite stream if the predicate is not defined for a given event" in testSample(
    sample = finiteStreamSample,
    elaboration = _.until({ case 2 => true }),
    expectation = Seq(E(0), E(1), EOS),
  )
  it should s"combine correctly with other $Until operators" in testSample(
    sample = finiteStreamSample,
    elaboration = _.until(_ > 2).until(_ > 1),
    expectation = Seq(E(0), E(1), EOS),
  )

  Take should "end a finite stream when it fires an event after a given number of events" in testSample(
    sample = finiteStreamSample,
    elaboration = _.take(2),
    expectation = Seq(E(0), E(1), EOS),
  )
  it should "end a finite stream if the stream ends on its own" in testSample(
    sample = finiteStreamSample,
    elaboration = _.take(10),
    expectation = Seq(E(0), E(1), E(2), E(3), EOS),
  )
  it should s"combine correctly with other $Take operators" in testSample(
    sample = finiteStreamSample,
    elaboration = _.take(10).take(2),
    expectation = Seq(E(0), E(1), EOS),
  )

  TakeBefore should
    "end a finite stream when an event is fired after a" +
    "certain duration since its call" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.takeBefore(200.milliseconds),
    expectation = Seq(E(0), E(1), EOS),
  )
  it should "end a finite stream if the stream ends on its own" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.takeBefore(800.milliseconds),
    expectation = Seq(E(0), E(1), E(2), E(3), EOS),
  )
  it should
    "not end a finite stream if the stream does not fire " +
    "any event after the specified duration" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.events.takeBefore(800.milliseconds),
    expectation = Seq(E(0), E(1), E(2), E(3)),
  )
  it should s"combine correctly with other $TakeBefore operators" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.takeBefore(800.milliseconds).takeBefore(200.milliseconds),
    expectation = Seq(E(0), E(1), EOS),
  )

  TakeBeforeInactivityOf should
    "end a stream when an event is fired after a " +
    "certain duration since the previous event" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.takeBeforeInactivityOf(200.milliseconds),
    expectation = Seq(E(0), E(1), E(2), EOS),
  )
  it should "end a finite stream if the stream ends on its own" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.takeBeforeInactivityOf(400.milliseconds),
    expectation = Seq(E(0), E(1), E(2), E(3), EOS),
  )
  it should
    "not end a finite stream if the stream does not fire " +
    "any event after the specified duration" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.events.takeBeforeInactivityOf(400.milliseconds),
    expectation = Seq(E(0), E(1), E(2), E(3)),
  )
  it should s"combine correctly with other $TakeBeforeInactivityOf operators" in testSample(
    sample = timedFiniteStreamSample,
    elaboration =
      _.takeBeforeInactivityOf(400.milliseconds)
       .takeBeforeInactivityOf(200.milliseconds),
    expectation = Seq(E(0), E(1), E(2), EOS),
  )

  InterruptBy should "end a finite stream when another stream fires an event" in testSample(
    sample = finiteTristreamSample,
    elaboration = (s1, s2, _) => s1.interruptBy(s2),
    expectation = Seq(E(0), E(1), EOS),
  )
  it should "end a finite stream if the stream ends on its own" in testSample(
    sample = finiteTristreamSample,
    elaboration = (s1, _, s3) => s1.interruptBy(s3),
    expectation = Seq(E(0), E(1), E(2), E(3), EOS),
  )
  it should s"combine correctly with other $InterruptBy operators" in testSample(
    sample = finiteTristreamSample,
    elaboration = (s1, s2, s3) => s1.interruptBy(s3).interruptBy(s2),
    expectation = Seq(E(0), E(1), EOS),
  )

  InterruptAfter should "end a finite stream after a certain duration since its call" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.interruptAfter(200.milliseconds).zipWithTime(),
    expectation = Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      EOS -> 200.milliseconds,
    ),
  )
  it should "end a finite stream if the stream ends on its own" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.interruptAfter(800.milliseconds).zipWithTime(),
    expectation = Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      E(3) -> 500.milliseconds,
      EOS -> 600.milliseconds,
    ),
  )
  it should
    "end a finite stream even if the stream does not fire " +
    "any event after the specified duration" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.events.interruptAfter(800.milliseconds).zipWithTime(),
    expectation = Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      E(3) -> 500.milliseconds,
      EOS -> 800.milliseconds,
    ),
  )
  it should s"combine correctly with other $InterruptAfter operators" in testSample(
    sample = timedFiniteStreamSample,
    elaboration =
      _.interruptAfter(800.milliseconds)
       .interruptAfter(200.milliseconds)
       .zipWithTime(),
    expectation = Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      EOS -> 200.milliseconds,
    ),
  )

  InterruptAfterInactivityOf should
    "end a finite stream after a certain duration since " +
    "its previous event" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.interruptAfterInactivityOf(200.milliseconds).zipWithTime(),
    expectation = Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      EOS -> 450.milliseconds,
    ),
  )
  it should "end a finite stream if the stream ends on its own" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.interruptAfterInactivityOf(800.milliseconds).zipWithTime(),
    expectation = Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      E(3) -> 500.milliseconds,
      EOS -> 600.milliseconds,
    ),
  )
  it should
    "end a finite stream even if the stream does not fire " +
    "any event after the specified duration" in testSample(
    sample = timedFiniteStreamSample,
    elaboration = _.events.interruptAfterInactivityOf(800.milliseconds).zipWithTime(),
    expectation = Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      E(3) -> 500.milliseconds,
      EOS -> 1300.milliseconds,
    ),
  )
  it should s"combine correctly with other $InterruptAfterInactivityOf operators" in testSample(
    sample = timedFiniteStreamSample,
    elaboration =
      _.interruptAfterInactivityOf(800.milliseconds)
       .interruptAfterInactivityOf(200.milliseconds)
       .zipWithTime(),
    expectation = Seq(
      E(0) -> 0.milliseconds,
      E(1) -> 150.milliseconds,
      E(2) -> 250.milliseconds,
      EOS -> 450.milliseconds,
    )
  )
