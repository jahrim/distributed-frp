package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.StreamExtension.*
import it.unibo.distributedfrp.frp.StreamSinkExtension.{*, given}
import it.unibo.distributedfrp.utils.Symbols
import nz.sodium
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/** Test for [[StreamSinkExtension]]. */
class StreamSinkExtensionTest
  extends AnyFlatSpec
    with should.Matchers
    with Symbols
    with StreamSampleLoader:
  private val SendShortcut = symbol("!")

  SendShortcut should "make a stream sink produce a sequence of events" in {
    val (sink, monitor) = Stream.monitor(sodium.StreamSink[Int]())
    sink ! 0
    sink ! (1, 2, 3)
    monitor.eventLog shouldEqual Seq(0, 1, 2, 3)
  }

  it should "make a pair of streams produce a sequence of pairs of simultaneous events" in {
    val (sink1, monitor1) = Stream.monitor(sodium.StreamSink[Int]())
    val (sink2, monitor2) = Stream.monitor(sodium.StreamSink[String]())
    val (_, andMonitor) = Stream.monitor(sink1 and sink2)
    sink1 ! (1, 2, 3)
    sink2 ! ("a", "b", "c")
    (sink1, sink2) ! ((4, "d"), (5, "e"))
    monitor1.eventLog shouldEqual Seq(1, 2, 3, 4, 5)
    monitor2.eventLog shouldEqual Seq("a", "b", "c", "d", "e")
    andMonitor.eventLog shouldEqual Seq((4, "d"), (5, "e"))
  }

  it should "make a triplet of streams produce a sequence of triplets of simultaneous events" in {
    def flattenToTriplet[A,B,C](pair: ((A,B),C)): (A,B,C) = (pair._1._1, pair._1._2, pair._2)
    val (sink1, monitor1) = Stream.monitor(sodium.StreamSink[Int]())
    val (sink2, monitor2) = Stream.monitor(sodium.StreamSink[String]())
    val (sink3, monitor3) = Stream.monitor(sodium.StreamSink[Boolean]())
    val (_, monitor12) = Stream.monitor(sink1 and sink2)
    val (_, monitor13) = Stream.monitor(sink1 and sink3)
    val (_, monitor23) = Stream.monitor(sink2 and sink3)
    val (_, monitor123) = Stream.monitor((sink1 and sink2 and sink3).map(flattenToTriplet(_)))

    sink1 ! (1, 2, 3)
    sink2 ! ("a", "b", "c")
    sink3 ! (true, false, true)
    (sink1, sink2) ! ((4, "d"), (5, "e"))
    (sink1, sink3) ! ((6, false), (7, true))
    (sink2, sink3) ! (("f", false), ("g", true))
    (sink1, sink2, sink3) ! ((8, "h", false), (9, "i", true))

    monitor1.eventLog shouldEqual Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)
    monitor2.eventLog shouldEqual Seq("a", "b", "c", "d", "e", "f", "g", "h", "i")
    monitor3.eventLog shouldEqual Seq(true, false, true, false, true, false, true, false, true)
    monitor12.eventLog shouldEqual Seq((4, "d"), (5, "e"), (8, "h"), (9, "i"))
    monitor13.eventLog shouldEqual Seq((6, false), (7, true), (8, false), (9, true))
    monitor23.eventLog shouldEqual Seq(("f", false), ("g", true), ("h", false), ("i", true))
    monitor123.eventLog shouldEqual Seq((8, "h", false), (9, "i", true))
  }
