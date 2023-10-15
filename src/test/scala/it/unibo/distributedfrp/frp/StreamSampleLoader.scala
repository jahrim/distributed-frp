package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.StreamSample

/** A mixin that allows to load [[StreamSample]]s within the inheriting class. */
trait StreamSampleLoader:
  /**
   * Load the specified [[StreamSample]], destructuring it into
   * its [[sodium.Stream Stream]]s.
   *
   * @param sample the specified [[StreamSample]].
   * @return the specified [[StreamSample]] itself and its
   *         [[sodium.Stream Stream]]s.
   */
  def load[SS](sample: StreamSample[SS]): (StreamSample[SS], SS) =
    (sample, sample.streams)
