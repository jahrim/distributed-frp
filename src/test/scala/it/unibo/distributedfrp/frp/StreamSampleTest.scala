package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.AbstractTest
import it.unibo.distributedfrp.frp.StreamExtension.Stream
import it.unibo.distributedfrp.frp.StreamSample

/** A test for [[Stream Stream]] functionalities, checked against [[StreamSample]]s. */
trait StreamSampleTest extends AbstractTest:
  /**
   * Create a custom test for a [[Stream Stream]].
   * The test succeeds if applying the specified elaboration to the streams
   * of the specified [[StreamSample]] yields a [[Stream Stream]] that fires
   * the specified expected sequence of events.
   *
   * @param sample      the specified [[StreamSample]].
   * @param elaboration the specified elaboration.
   * @param expectation the specified expected sequence of events.
   * @tparam A  the type of [[Stream Stream]]
   * @tparam SS the type of stream (or streams) of the [[StreamSample]].
   */
  protected def testSample[A, SS](
    sample: StreamSample[SS],
    elaboration: SS => Stream[A],
    expectation: Seq[A]
  ): Unit =
    val streamMonitor = Stream.monitor(elaboration(sample.streams))
    sample.generateEvents()
    streamMonitor.eventLog shouldEqual expectation

  /**
   * Load the specified [[StreamSample]], destructuring it into
   * its [[Stream Stream]]s.
   *
   * @param sample the specified [[StreamSample]].
   * @return the specified [[StreamSample]] itself and its
   *         [[Stream Stream]]s.
   */
  protected def loadSample[SS](sample: StreamSample[SS]): (StreamSample[SS], SS) =
    (sample, sample.streams)
