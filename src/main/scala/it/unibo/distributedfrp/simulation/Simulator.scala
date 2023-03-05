package it.unibo.distributedfrp.simulation

import it.unibo.distributedfrp.core.Incarnation
import nz.sodium.Transaction

import java.util.concurrent.{ExecutorService, Executors}

class Simulator(val incarnation: SimulationIncarnation, executor: ExecutorService = Executors.newSingleThreadExecutor):

  import incarnation._

  def run[A](flow: Flow[A]): Unit =
    val contexts = for (i <- 0 until incarnation.environment.nDevices) yield context(i)
    val exports = Transaction.run(() => contexts.map(ctx => (ctx.selfId, flow.run(Seq.empty)(using ctx))))
    exports.foreach((id, exp) => exp.listen(e => {
      println(s"Device $id exported:\n$e")
      executor.execute(() => deviceExported(id, e, contexts))
    }))

  private def deviceExported[A](id: DeviceId, exported: Export[A], contexts: Seq[Context]): Unit =
    incarnation.environment.neighbors(id).foreach { n =>
      contexts(n).neighborExported(id, exported)
    }

