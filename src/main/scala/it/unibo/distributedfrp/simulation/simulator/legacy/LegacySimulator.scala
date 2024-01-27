package it.unibo.distributedfrp.simulation.simulator.legacy

import it.unibo.distributedfrp.frp.IncrementalCellSink
import it.unibo.distributedfrp.simulation.incarnation.SimulationIncarnation
import nz.sodium.Transaction

import java.util.concurrent.{ExecutorService, Executors}

class LegacySimulator[I <: SimulationIncarnation](
  val incarnation: I,
  executor: ExecutorService = Executors.newSingleThreadExecutor
):
  import incarnation.*

  def run[A](flow: Flow[A])(using environment: Environment): Unit =
    val ctxsNbrs = Seq.fill(environment.nDevices)(IncrementalCellSink(Map[DeviceId, NeighborState](), calm = true))
    val ctxs = ctxsNbrs.zipWithIndex.map((neighbors, id) => SimulationContext(id, neighbors.cell, environment))
    Transaction.runVoid(() => {
      val exports = ctxs.map(ctx => (ctx.selfId, flow.run(Seq.empty)(using ctx)))
      exports.foreach((id, exp) => exp.listen(e => {
        println(s"Device $id exported:\n$e")
        environment.neighbors(id).foreach { n =>
          executor.execute(() => ctxsNbrs(n).update(nn =>
            nn + (id -> SimulationNeighborState(n, id, e, environment))
          ))
        }
      }))
    })
