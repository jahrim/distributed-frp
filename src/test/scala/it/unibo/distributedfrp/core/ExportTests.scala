package it.unibo.distributedfrp.core

import org.scalatest._
import flatspec._
import matchers._

class ExportTests extends AnyFlatSpec with should.Matchers:

  "A generic export" should "have the given root" in {
    Export(10, Map.empty).root should be (10)
  }

  it should "have the given children" in {
    val children = Map[Any, Export[Any]](
      "a" -> Export.atomic(1),
      "b" -> Export.atomic(2),
    )
    Export(10, children).children should be (children)
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
