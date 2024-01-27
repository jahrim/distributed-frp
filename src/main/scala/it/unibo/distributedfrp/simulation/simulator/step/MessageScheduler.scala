package it.unibo.distributedfrp.simulation.simulator.step

import it.unibo.distributedfrp.simulation.simulator.Simulator
import it.unibo.distributedfrp.utils.Observables.{EventBus, Observable}
import it.unibo.distributedfrp.utils.Scheduler

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

/**
 * A mixin for providing the concept of a [[Scheduler]] for the device
 * messages in a simulation to a [[Simulator Simulator]].
 */
trait MessageScheduler:
  self: Simulator =>
  import incarnation.{*, given}

  /**
   * A thread-safe [[Scheduler]] for device messages, managing the order by
   * which device messages are handled in a simulation.
   *
   * @param devices the devices in the simulation.
   * @tparam A the type of results produced by the simulation.
   */
  protected class MessageScheduler[A](devices: Map[DeviceId, Context]) extends Scheduler[(DeviceId, Export[A])]:
    private case class Message(sender: DeviceId, content: Export[A], receipt: Promise[Unit])

    private object ReadyEventBus extends EventBus[Unit]
    private val _deviceOutgoingMessagesMap: Map[DeviceId, java.util.Queue[Message]] =
      devices.map[DeviceId, java.util.Queue[Message]](_._1 -> java.util.concurrent.ConcurrentLinkedQueue())
    private val _deviceScheduler: Iterator[Int] = LazyList.iterate(0)(id => (id + 1) % devices.size).iterator
    private var _deviceReadyMap: Map[DeviceId, Boolean] = devices.map(_._1 -> true)

    override def schedule(resource: (DeviceId, Export[A])): Future[Unit] =
      val message: Message = Message(sender = resource._1, content = resource._2, receipt = Promise())
      this.enqueue(message)
      message.receipt.future

    override def next[B](consumer: Option[(DeviceId, Export[A])] => B)(using
      executor: ExecutionContext = ExecutionContext.parasitic
    ): Future[B] =
      Future {
        val nextMessage: Option[Message] = this.dequeue()
        val tryResult: Try[B] = Try(consumer(nextMessage.map(message => message.sender -> message.content)))
        nextMessage.foreach(message =>
          message.receipt.complete(tryResult.map(_ => ()))
          this.setReady(message.sender)
        )
        tryResult.get
      }

    /**
     * @return an [[Observable Observable]] firing events each time a new
     *         resource is available to be consumed.
     */
    def ready: Observable[Unit] = ReadyEventBus

    /**
     * Add the specified [[Message Message]] to the queue of its sender.
     *
     * @param message the specified [[Message Message]].
     */
    private def enqueue(message: Message): Unit = synchronized {
      this._deviceOutgoingMessagesMap(message.sender).offer(message)
      if this._deviceReadyMap(message.sender) then ReadyEventBus.publish(())
    }

    /**
     * Remove the next [[Message Message]] to be consumed from the queue
     * of its sender.
     *
     * @return an [[Option]] containing the next [[Message Message]] to be
     *         consumed, if there are any that can be consumed.
     */
    private def dequeue(): Option[Message] = synchronized {
      LazyList.range(0, this.devices.size)
        .map(_ => this._deviceScheduler.next())
        .find(sender => this._deviceReadyMap(sender) && !this._deviceOutgoingMessagesMap(sender).isEmpty)
        .flatMap(sender =>
          val message: Option[Message] = Option(this._deviceOutgoingMessagesMap(sender).poll())
          this.setBusy(sender)
          message
        )
    }

    /**
     * Allow dequeueing [[Message Message]]s from the queue of the specified sender.
     *
     * @param sender the specified sender.
     */
    private def setReady(sender: DeviceId): Unit = synchronized {
      this._deviceReadyMap = this._deviceReadyMap + (sender -> true)
      if !this._deviceOutgoingMessagesMap(sender).isEmpty then ReadyEventBus.publish(())
    }

    /**
     * Forbid dequeueing [[Message Message]]s from the queue of the specified sender.
     *
     * @param sender the specified sender.
     */
    private def setBusy(sender: DeviceId): Unit = synchronized {
      this._deviceReadyMap = this._deviceReadyMap + (sender -> false)
    }
