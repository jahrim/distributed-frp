package it.unibo.distributedfrp.core

import org.scalatest.*
import flatspec.*
import matchers.*
import nz.sodium.{Cell, StreamSink, Transaction}
import it.unibo.distributedfrp.utils.Lift.*

class SemanticsTests extends AnyFlatSpec with should.Matchers:
  private val SELF_ID = 1
  private val PATH = Seq.empty

  private val LOCAL_SENSOR = "A"
  private val NBR_SENSOR = "NBR_SENSOR"

  private val initialSensorValues = Map(
    LOCAL_SENSOR -> "A",
  )

  object SemanticsTestsIncarnation extends MockIncarnation:
    override def initialLocalSensors(selfId: DeviceId): Map[SensorId, Any] = initialSensorValues
    override def initialNeighborSensors(selfId: DeviceId, neighborId: DeviceId): Map[SensorId, Any] = Map(
      NBR_SENSOR -> nbrSensorValue(selfId, neighborId)
    )
    
    def nbrSensorValue(selfId: DeviceId, neighborId: DeviceId): String = s"$selfId -> $neighborId"

  import SemanticsTestsIncarnation._
  import SemanticsTestsIncarnation.given

  private given ctx: Context = context(SELF_ID)

  "mid" should "be a constant flow with the device ID" in {
    mid.exports(PATH).sample() should be (Export(SELF_ID))
  }
  
  "sensor" should "evaluate to the initial sensor value" in {
    sensor[String](LOCAL_SENSOR).exports(PATH).sample() should be (Export(initialSensorValues(LOCAL_SENSOR)))
  }

  it should "change according to sensor changes" in {
    val sensorExports = sensor[String](LOCAL_SENSOR).exports(PATH)
    val newValue = "B"
    ctx.updateLocalSensor(LOCAL_SENSOR)(newValue)
    sensorExports.sample() should be (Export(newValue))
  }

  "branch" should "include only the 'then' branch when the condition is true" in {
    val thenValue = 1
    val elseValue = 2
    val branchFlow = branch(Flows.constant(true))(Flows.constant(thenValue))(Flows.constant(elseValue))
    branchFlow.exports(PATH).sample() should be (Export(
      thenValue,
      () -> Export(true),
      true -> Export(thenValue)
    ))
  }

  it should "include only the 'else' branch when the condition is false" in {
    val thenValue = 1
    val elseValue = 2
    val branchFlow = branch(Flows.constant(false))(Flows.constant(thenValue))(Flows.constant(elseValue))
    branchFlow.exports(PATH).sample() should be(Export(
      elseValue,
      () -> Export(false),
      false -> Export(elseValue)
    ))
  }

  "nbr" should "collect values from aligned neighbors" in {
    val flow = branch(mid.map(_ < 3))(nbr(mid))(nbr(Flows.constant(0)))
    val neighbors = Set(SELF_ID, SELF_ID + 1, SELF_ID + 2, SELF_ID + 3)
    neighbors.foreach { n =>
      val nbrContext = context(n)
      ctx.addNeighbor(n)(flow.exports(PATH)(using nbrContext).sample())
    }
    val expectedNeighborField = NeighborField(neighbors.filter(_ < 3).map(x => (x, x)).toMap)
    val expectedExport = Export(
      expectedNeighborField,
      () -> Export(
        true,
        () -> Export(SELF_ID)
      ),
      true -> Export(
        expectedNeighborField,
        () -> Export(SELF_ID)
      )
    )
    flow.exports(PATH).sample() should be (expectedExport)
  }

  "nbrSensor" should "collect sensors from aligned neighbors" in {
    val nbrSensorId = "NBR_SENSOR"
    val flow = branch(mid.map(_ < 3))(nbrSensor(nbrSensorId))(nbr(Flows.constant(0)))
    val neighbors = Set(1, 2, 3, 4)
    neighbors.foreach { n =>
      val nbrContext = context(n)
      ctx.addNeighbor(n)(flow.exports(PATH)(using nbrContext).sample())
    }
    val expectedNeighborField = NeighborField(neighbors.filter(_ < 3).map(x => (x, nbrSensorValue(SELF_ID, x))).toMap)
    val expectedExport = Export(
      expectedNeighborField,
      () -> Export(
        true,
        () -> Export(SELF_ID)
      ),
      true -> Export(
        expectedNeighborField
      )
    )
    flow.exports(PATH).sample() should be (expectedExport)
  }

  "loop" should "return a flow self-dependant flow" in {
    val s = sensor[String](LOCAL_SENSOR)
    val clock = new StreamSink[Unit]
    val flow = loop[String](clock)("")(x => lift(s, x)(_ + _))
    val exports = flow.exports(PATH)
    exports.sample().root should be (initialSensorValues(LOCAL_SENSOR))
    clock.send(())
    exports.sample().root should be (initialSensorValues(LOCAL_SENSOR) + initialSensorValues(LOCAL_SENSOR))
  }
