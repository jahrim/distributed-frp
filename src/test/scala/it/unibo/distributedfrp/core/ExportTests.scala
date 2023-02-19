package it.unibo.distributedfrp.core

import org.scalatest.*
import flatspec.*
import matchers.*
import it.unibo.distributedfrp.core.Slot._

import scala.annotation.targetName

class ExportTests extends AnyFlatSpec with should.Matchers:
  "An export" should "have the given root" in {
    Export(1).root should be (1)
  }

  it should "have the given children" in {
    val children: Seq[(Slot, Export[Any])] = Seq(
      Key("a") -> Export(10),
      Key("b") -> Export(20),
    )
    Export(1, children: _*).children should be (children.toMap)
  }

  it should "be traversable in zero steps" in {
    val exp = Export(1)
    exp.followPath(Seq.empty) should be (Some(exp))
  }

  it should "be traversable to its immediate children" in {
    val child = Export(10)
    val exp = Export(1,
      Key("a") -> child,
      Key("b") -> Export(2)
    )
    exp.followPath(Seq(Key("a"))) should be (Some(child))
  }

  it should "be traversable to nested children" in {
    val child = Export(15)
    val exp = Export(
      1,
      Key("a") -> Export(
        2,
        Key("aa") -> Export(
          3,
          Key("aaa") -> child,
          Key("aab") -> Export(4),
        ),
        Key("ab") -> Export(5),
      ),
      Key("b") -> Export(6)
    )
    exp.followPath(Seq(Key("a"), Key("aa"), Key("aaa"))) should be(Some(child))
  }

  it should "not be traversable with an invalid path" in {
    val exp = Export(
      1,
      Key("a") -> Export(
        2,
        Key("aa") -> Export(3),
        Key("ab") -> Export(4),
      ),
      Key("b") -> Export(5)
    )
    exp.followPath(Seq(Key("c"))) should be (None)
    exp.followPath(Seq(Key("a"), Key("ac"))) should be (None)
    exp.followPath(Seq(Key("a"), Key("aa"), Key("aaa"))) should be (None)
  }
