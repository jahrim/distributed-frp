package it.unibo.distributedfrp.utils

import it.unibo.distributedfrp.AbstractTest
import it.unibo.distributedfrp.utils.Observables.*

import scala.concurrent.{Await, Promise}

/** Test for [[Observables]]. */
class ObservableTest extends AbstractTest:
  private val Observable = symbol("Observable")

  Observable should "let a consumer subscribe to its events and a producer publish new events" in {
    val eventBus: EventBus[Int] = new EventBus[Int]{} // Producer Perspective
    val observable: Observable[Int] = eventBus        // Consumer Perspective
    val product: Int = 0
    val eventReceived: Promise[Unit] = Promise[Unit]()

    observable.subscribe(event =>
      if event == product
      then eventReceived.success(())
      else fail("Consumer did not receive the event published by the producer.")
    )
    eventBus.publish(product)
    Await.result(eventReceived.future, Defaults.timeout)
  }

  it should "let a consumer cancel its subscriptions" in {
    val eventBus: EventBus[Unit] = new EventBus[Unit]{}
    val observable: Observable[Unit] = eventBus
    val eventReceived: Promise[Unit] = Promise[Unit]()

    val subscription1 = observable.subscribe(_ => fail("Subscription was not cancelled."))
    val subscription2 = observable.subscribe(_ => eventReceived.success(()))
    subscription1.unsubscribe()
    eventBus.publish(())
    Await.result(eventReceived.future, Defaults.timeout)
  }
