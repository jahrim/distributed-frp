package it.unibo.distributedfrp.core

import it.unibo.distributedfrp.AbstractTest
import it.unibo.distributedfrp.core.Slot.*

class ExportTreeTest extends AbstractTest:
  "An export tree" should "have the given root" in {
    ExportTree(1).root should be (1)
  }

  it should "have the given children" in {
    val children: Seq[(Slot, ExportTree[Any])] = Seq(
      Key("a") -> ExportTree(10),
      Key("b") -> ExportTree(20),
    )
    ExportTree(1, children: _*).children should be (children.toMap)
  }

  it should "be traversable in zero steps" in {
    val exp = ExportTree(1)
    exp.followPath(Seq.empty) should be (Some(exp))
  }

  it should "be traversable to its immediate children" in {
    val child = ExportTree(10)
    val exp = ExportTree(1,
      Key("a") -> child,
      Key("b") -> ExportTree(2)
    )
    exp.followPath(Seq(Key("a"))) should be (Some(child))
  }

  it should "be traversable to nested children" in {
    val child = ExportTree(15)
    val exp = ExportTree(
      1,
      Key("a") -> ExportTree(
        2,
        Key("aa") -> ExportTree(
          3,
          Key("aaa") -> child,
          Key("aab") -> ExportTree(4),
        ),
        Key("ab") -> ExportTree(5),
      ),
      Key("b") -> ExportTree(6)
    )
    exp.followPath(Seq(Key("a"), Key("aa"), Key("aaa"))) should be(Some(child))
  }

  it should "not be traversable with an invalid path" in {
    val exp = ExportTree(
      1,
      Key("a") -> ExportTree(
        2,
        Key("aa") -> ExportTree(3),
        Key("ab") -> ExportTree(4),
      ),
      Key("b") -> ExportTree(5)
    )
    exp.followPath(Seq(Key("c"))) should be (None)
    exp.followPath(Seq(Key("a"), Key("ac"))) should be (None)
    exp.followPath(Seq(Key("a"), Key("aa"), Key("aaa"))) should be (None)
  }
