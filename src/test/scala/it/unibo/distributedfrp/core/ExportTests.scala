package it.unibo.distributedfrp.core

import org.scalatest._
import flatspec._
import matchers._

class ExportTests extends AnyFlatSpec with should.Matchers:

  "A generic export" should "have the given root" in {
    Export(1, Map.empty).root should be (1)
  }

  it should "have the given children" in {
    val children: Map[Any, Export[Any]] = Map(
      "a" -> Export.atomic(10),
      "b" -> Export.atomic(20),
    )
    Export(1, children).children should be (children)
  }

  it should "be traversable in zero steps" in {
    val exp = Export(1, Map.empty)
    exp.followPath(Seq.empty) should be (Some(exp))
  }

  it should "be traversable to its immediate children" in {
    val child = Export.atomic(10)
    val exp = Export(1, Map(
      "a" -> child,
      "b" -> Export.atomic(2)
    ))
    exp.followPath(Seq("a")) should be (Some(child))
  }

  it should "be traversable to nested children" in {
    val child = Export.atomic(15)
    val exp = Export(1, Map(
      "a" -> Export(2, Map(
        "aa" -> Export(3, Map(
          "aaa" -> child,
          "aab" -> Export.atomic(4),
        )),
        "ab" -> Export.atomic(5),
      )),
      "b" -> Export.atomic(6)
    ))
    exp.followPath(Seq("a", "aa", "aaa")) should be(Some(child))
  }

  it should "not be traversable with an invalid path" in {
    val exp = Export(1, Map(
      "a" -> Export(2, Map(
        "aa" -> Export.atomic(3),
        "ab" -> Export.atomic(4),
      )),
      "b" -> Export.atomic(5)
    ))
    exp.followPath(Seq("c")) should be (None)
    exp.followPath(Seq("a", "ac")) should be (None)
    exp.followPath(Seq("a", "aa", "aaa")) should be (None)
  }

  "An atomic export" should "have the given root" in {
    Export.atomic(10).root should be (10)
  }

  it should "have no children" in {
    Export.atomic(10).children shouldBe empty
  }

  "A wrapper export" should "have the given root" in {
    Export.wrapper(10, "A", Export.atomic(15)).root shouldBe 10
  }

  it should "have the given export as its only child" in {
    val key = "A"
    val child = Export.atomic(15)
    Export.wrapper(10, key, child).children should be (Map(key -> child))
  }
