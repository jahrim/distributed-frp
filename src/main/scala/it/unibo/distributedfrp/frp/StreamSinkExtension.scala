package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.frp.StreamExtension.Stream
import nz.sodium
import scala.annotation.targetName

/** An extension for [[sodium.StreamSink StreamSink]]s. */
object StreamSinkExtension:
  /** A [[Stream Stream]] to which the user can push events. */
  type StreamSink[A] = sodium.StreamSink[A]

  extension[A] (self: StreamSink[A]) {
    /**
     * Send the specified events to this [[StreamSink StreamSink]].
     *
     * @param events the specified events.
     */
    @targetName("sendShortcut")
    def !(events: A*): Unit =
      events.foreach(self.send)
  }

  extension[A, B] (self: (StreamSink[A], StreamSink[B])) {
    /**
     * Send the specified simultaneous events to these [[StreamSink StreamSink]]s.
     *
     * @param events the specified simultaneous events.
     */
    @targetName("sendShortcut")
    def !(events: (A, B)*): Unit =
      events.foreach((a, b) =>
        sodium.Transaction.run(() => {
          self._1 ! a
          self._2 ! b
        })
      )
  }

  extension[A, B, C] (self: (StreamSink[A], StreamSink[B], StreamSink[C])) {
    /**
     * Send the specified simultaneous events to these [[StreamSink StreamSink]]s.
     *
     * @param events the specified simultaneous events.
     */
    @targetName("sendShortcut")
    def !(events: (A, B, C)*): Unit =
      events.foreach((a, b, c) =>
        sodium.Transaction.run(() => {
          self._1 ! a
          self._2 ! b
          self._3 ! c
        })
      )
  }