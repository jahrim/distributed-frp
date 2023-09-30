package it.unibo.distributedfrp.frp

import nz.sodium
import scala.annotation.targetName

/** An extension for [[sodium.StreamSink StreamSink]]s. */
object StreamSinkExtension:
  extension[A] (self: sodium.StreamSink[A]) {
    /**
     * Send the specified events to this [[sodium.StreamSink StreamSink]].
     *
     * @param events the specified events.
     */
    @targetName("sendShortcut")
    def !(events: A*): Unit =
      events.foreach(self.send)
  }

  extension[A, B] (self: (sodium.StreamSink[A], sodium.StreamSink[B])) {
    /**
     * Send the specified simultaneous events to these [[sodium.StreamSink StreamSink]]s.
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

  extension[A, B, C] (self: (sodium.StreamSink[A], sodium.StreamSink[B], sodium.StreamSink[C])) {
    /**
     * Send the specified simultaneous events to these [[sodium.StreamSink StreamSink]]s.
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