package it.unibo.distributedfrp.core

import org.scalatest.*
import flatspec.*
import matchers.*

import scala.annotation.targetName

class ExportTests extends AnyFlatSpec with should.Matchers:
  "An export" should "have the given root" in {
    Export(1).root should be (1)
  }

  it should "have the given children" in {
    val children: Seq[(Any, Export[Any])] = Seq(
      "a" -> Export(10),
      "b" -> Export(20),
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
      "a" -> child,
      "b" -> Export(2)
    )
    exp.followPath(Seq("a")) should be (Some(child))
  }

  it should "be traversable to nested children" in {
    val child = Export(15)
    val exp = Export(
      1,
      "a" -> Export(
        2,
        "aa" -> Export(
          3,
          "aaa" -> child,
          "aab" -> Export(4),
        ),
        "ab" -> Export(5),
      ),
      "b" -> Export(6)
    )
    exp.followPath(Seq("a", "aa", "aaa")) should be(Some(child))
  }

  it should "not be traversable with an invalid path" in {
    val exp = Export(
      1,
      "a" -> Export(
        2,
        "aa" -> Export(3),
        "ab" -> Export(4),
      ),
      "b" -> Export(5)
    )
    exp.followPath(Seq("c")) should be (None)
    exp.followPath(Seq("a", "ac")) should be (None)
    exp.followPath(Seq("a", "aa", "aaa")) should be (None)
  }
