package it.unibo.distributedfrp.simulation.simulator

import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation
import nz.sodium.Transaction

import java.util.concurrent.{ExecutorService, Executors}

class LegacySimulator[I <: SimulationIncarnation](
  val incarnation: I,
  executor: ExecutorService = Executors.newSingleThreadExecutor
):
  import incarnation.*

  def run[A](flow: Flow[A])(using environment: Environment): Unit =
    val contexts = (0 until environment.nDevices).map(id => context(id, environment))
    Transaction.runVoid(() => {
      val exports = contexts.map(ctx => (ctx.selfId, flow.run(Seq.empty)(using ctx)))
      exports.foreach((id, exp) => exp.listen(e => {
        println(s"Device $id exported:\n$e")
        environment.neighbors(id).foreach { n =>
          executor.execute(() => contexts(n).receiveExport(id, e))
        }
      }))
    })
