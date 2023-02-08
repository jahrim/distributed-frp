package it.unibo.distributedfrp.core

import org.scalatest.*
import flatspec.*
import matchers.*
import nz.sodium.Cell

class SemanticsTests extends AnyFlatSpec with should.Matchers:
  import MockIncarnation._
  import MockIncarnation.given

  private val SELF_ID = 1
  private val PATH = Seq.empty

  private val SENSOR_A = "A"
  private val SENSOR_B = "B"

  private val initialSensorValues = Map(
    SENSOR_A -> "A",
    SENSOR_B -> 1
  )

  private given ctx: Context = MockContext(SELF_ID, initialSensorValues)

  "mid" should "be a constant flow with the device ID" in {
    mid.exports(PATH).sample() should be (Export(SELF_ID))
  }
  
  "sensor" should "evaluate to the initial sensor value" in {
    sensor[String](SENSOR_A).exports(PATH).sample() should be (Export(initialSensorValues(SENSOR_A)))
  }

  it should "change according to sensor changes" in {
    val sensorExports = sensor[String](SENSOR_A).exports(PATH)
    val newValue = "B"
    ctx.updateLocalSensor(SENSOR_A)(newValue)
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
      val nbrContext = MockContext(n, initialSensorValues)
      ctx.addNeighbor(n)(flow.exports(PATH)(using nbrContext).sample(), Map.empty)
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
      val nbrContext = MockContext(n, initialSensorValues)
      ctx.addNeighbor(n)(flow.exports(PATH)(using nbrContext).sample(), Map(nbrSensorId -> n.toString))
    }
    val expectedNeighborField = NeighborField(neighbors.filter(_ < 3).map(x => (x, x.toString)).toMap)
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
