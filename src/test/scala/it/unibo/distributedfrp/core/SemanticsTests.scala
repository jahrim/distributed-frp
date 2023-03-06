package it.unibo.distributedfrp.core

import org.scalatest.*
import flatspec.*
import matchers.*
import nz.sodium.{Cell, CellSink, StreamSink, Transaction}
import it.unibo.distributedfrp.utils.Liftable.*
import it.unibo.distributedfrp.core.Slot.*

class SemanticsTests extends AnyFlatSpec with should.Matchers with BeforeAndAfterEach:
  private val SELF_ID = 1
  private val NEIGHBORS = Set(1, 2, 3, 4)
  private val PATH = Seq.empty

  private val LOCAL_SENSOR = "A"
  private val NBR_SENSOR = "NBR_SENSOR"

  private val THEN_VALUE = 1
  private val ELSE_VALUE = 2

  private val initialSensorValues = Map(
    LOCAL_SENSOR -> "A",
  )

  object SemanticsTestsIncarnation extends MockIncarnation:
    override def initialLocalSensors(selfId: DeviceId): Map[LocalSensorId, Any] = initialSensorValues
    override def initialNeighborSensors(selfId: DeviceId, neighborId: DeviceId): Map[NeighborSensorId, Any] = Map(
      NBR_SENSOR -> nbrSensorValue(selfId, neighborId)
    )

    def nbrSensorValue(selfId: DeviceId, neighborId: DeviceId): String = s"$selfId -> $neighborId"

  import SemanticsTestsIncarnation._
  import SemanticsTestsIncarnation.given

  private given ctx: Context = context(SELF_ID)

  override def beforeEach(): Unit = ctx.reset()

  def runFlowOnNeighbors[A](flow: Flow[A], neighbors: Iterable[DeviceId] = NEIGHBORS): Unit =
    neighbors.foreach { n =>
      val nbrContext = context(n)
      ctx.receiveExportFromNeighbor(n, flow.run(PATH)(using nbrContext).sample())
    }

  "constant" should "be a constant flow with the given value" in {
    val value = 10
    constant(value).run(PATH).sample() should be (ExportTree(value))
  }

  "mid" should "be a constant flow with the device ID" in {
    mid.run(PATH).sample() should be (ExportTree(SELF_ID))
  }
  
  "sensor" should "evaluate to the initial sensor value" in {
    sensor[String](LOCAL_SENSOR).run(PATH).sample() should be (ExportTree(initialSensorValues(LOCAL_SENSOR)))
  }

  it should "react to sensor changes" in {
    val sensorExports = sensor[String](LOCAL_SENSOR).run(PATH)
    val newValue = "B"
    ctx.updateLocalSensor(LOCAL_SENSOR, newValue)
    sensorExports.sample().root should be (newValue)
  }

  "map" should "wrap the export using the given function" in {
    constant(0).map(_ + 1).run(PATH).sample() should be (ExportTree(
      1,
      Operand(0) -> ExportTree(0)
    ))
  }

  it should "react to source changes" in {
    val cell = new CellSink(0)
    val exports = Flows.fromCell(cell).map(_ + 1).run(PATH)
    cell.send(10)
    exports.sample().root should be (11)
  }

  "lift" should "combine two flows by nesting them" in {
    val left = "LEFT"
    val right = "RIGHT"
    val flow = lift(constant(left), constant(right))(_ + _)
    flow.run(PATH).sample() should be (ExportTree(
      left + right,
      Operand(0) -> ExportTree(left),
      Operand(1) -> ExportTree(right),
    ))
  }

  it should "react to changes in either its inputs" in {
    val stringOp = new CellSink("A")
    val intOp = new CellSink(2)
    val flow = lift(Flows.fromCell(stringOp), Flows.fromCell(intOp))(_ * _)
    val exports = flow.run(PATH)
    exports.sample().root should be ("AA")
    stringOp.send("B")
    exports.sample().root should be ("BB")
    intOp.send(3)
    exports.sample().root should be ("BBB")
  }

  "branch" should "include only the 'then' branch when the condition is true" in {
    val flow = branch(constant(true))(constant(THEN_VALUE))(constant(ELSE_VALUE))
    flow.run(PATH).sample() should be (ExportTree(
      THEN_VALUE,
      Condition -> ExportTree(true),
      Then -> ExportTree(THEN_VALUE)
    ))
  }

  it should "include only the 'else' branch when the condition is false" in {
    val flow = branch(constant(false))(constant(THEN_VALUE))(constant(ELSE_VALUE))
    flow.run(PATH).sample() should be (ExportTree(
      ELSE_VALUE,
      Condition -> ExportTree(false),
      Else -> ExportTree(ELSE_VALUE)
    ))
  }

  it should "react to changes in the condition" in {
    val condition = new CellSink(true)
    val flow = branch(Flows.fromCell(condition))(constant(THEN_VALUE))(constant(ELSE_VALUE))
    val exports = flow.run(PATH)
    condition.send(false)
    exports.sample().root should be (ELSE_VALUE)
  }

  it should "react to changes in the selected branch" in {
    val thenBranch = new CellSink(THEN_VALUE)
    val flow = branch(constant(true))(Flows.fromCell(thenBranch))(constant(ELSE_VALUE))
    val exports = flow.run(PATH)
    val newValue = 100
    thenBranch.send(newValue)
    exports.sample().root should be (newValue)
  }

  "mux" should "include both branches when the condition is true" in {
    val flow = mux(constant(true))(constant(THEN_VALUE))(constant(ELSE_VALUE))
    flow.run(PATH).sample() should be (ExportTree(
      THEN_VALUE,
      Condition -> ExportTree(true),
      Then -> ExportTree(THEN_VALUE),
      Else -> ExportTree(ELSE_VALUE)
    ))
  }

  it should "include both branches when the condition is false" in {
    val flow = mux(constant(false))(constant(THEN_VALUE))(constant(ELSE_VALUE))
    flow.run(PATH).sample() should be(ExportTree(
      ELSE_VALUE,
      Condition -> ExportTree(false),
      Then -> ExportTree(THEN_VALUE),
      Else -> ExportTree(ELSE_VALUE)
    ))
  }

  "nbr" should "collect values from aligned neighbors" in {
    val flow = branch(mid.map(_ < 3))(nbr(mid))(nbr(constant(0)))
    runFlowOnNeighbors(flow)
    val expectedNeighborField = NEIGHBORS.filter(_ < 3).map(x => (x, x)).toMap
    flow.run(PATH).sample().followPath(Seq(Then)).get should be (ExportTree(
      expectedNeighborField,
      Nbr -> ExportTree(SELF_ID)
    ))
  }

  it should "react to changes in the neighborhood state" in {
    val flow = nbr(sensor[String](LOCAL_SENSOR))
    val exports = flow.run(PATH)
    runFlowOnNeighbors(flow)
    exports.sample().root should be (NEIGHBORS.map((_, initialSensorValues(LOCAL_SENSOR))).toMap)
  }

  "nbrSensor" should "collect sensors from aligned neighbors" in {
    val flow = branch(mid.map(_ < 3))(nbrSensor(NBR_SENSOR))(nbr(constant(0)))
    runFlowOnNeighbors(flow)
    val expectedNeighborField = NEIGHBORS.filter(_ < 3).map(x => (x, nbrSensorValue(SELF_ID, x))).toMap
    flow.run(PATH).sample().followPath(Seq(Then)).get should be (ExportTree(
      expectedNeighborField
    ))
  }

  it should "react to neighboring sensor changes" in {
    val flow = nbrSensor[String](NBR_SENSOR)
    runFlowOnNeighbors(flow)
    val exports = flow.run(PATH)
    val updatedNeighbor = 2
    val newValue = "Hello"
    val initialNeighborField = exports.sample().root
    ctx.updateSensorForNeighbor(updatedNeighbor, NBR_SENSOR, newValue)
    exports.sample() should be (ExportTree(
      initialNeighborField.withNeighbor(updatedNeighbor, newValue)
    ))
  }

  "loop" should "return a self-dependant flow" in {
    val s = sensor[String](LOCAL_SENSOR)
    val flow = loop("")(x => lift(s, x)(_ + _))
    val exports = flow.run(PATH)
    exports.sample().root should be (initialSensorValues(LOCAL_SENSOR))
  }

  it should "react to updates in its past state" in {
    val s = sensor[String](LOCAL_SENSOR)
    val flow = loop("")(x => lift(s, x)(_ + _))
    val exports = flow.run(PATH)
    val sensorValue = initialSensorValues(LOCAL_SENSOR)
    ctx.receiveExportFromNeighbor(SELF_ID, exports.sample())
    exports.sample().root should be (sensorValue * 2)
    ctx.receiveExportFromNeighbor(SELF_ID, exports.sample())
    exports.sample().root should be (sensorValue * 3)
  }

  it should "react to updates in dependencies in the looping function" in {
    val s = sensor[String](LOCAL_SENSOR)
    val flow = loop("")(x => lift(s, x)(_ + _))
    val exports = flow.run(PATH)
    val newValue = "B"
    ctx.updateLocalSensor(LOCAL_SENSOR, newValue)
    exports.sample().root should be (newValue)
  }
