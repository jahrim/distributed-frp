package it.unibo.distributedfrp.test.utils.collections

import it.unibo.distributedfrp.test.utils.AbstractTest
import it.unibo.distributedfrp.test.utils.collections.Table
import it.unibo.distributedfrp.test.utils.collections.Table.*

/** Test for [[Table]]. */
class TableTest extends AbstractTest:
  private val FromRows = symbol("fromRows")
  private val FromContent = symbol("fromContent")
  private val Empty = symbol("empty")
  private val Content = symbol("content")
  private val Rows = symbol("rows")
  private val Columns = symbol("columns")
  private val Cells = symbol("cells")
  private val CellAt = symbol("cellAt")
  private val Transposed = symbol("transposed")
  private val WithTransposed = symbol("withTransposed")

  import TableTest.Samples.*

  FromRows should "create an empty table if no rows are specified" in {
    Table.fromRows(Seq.empty) should be(empty)
  }
  it should "create a table with the specified rows" in {
    val table = Table.fromRows(rows = Seq(
      "A" -> Map(0 -> 10, 1 -> 20, 2 -> 30),
      "B" -> Map(0 -> 40, 1 -> 50),
    ))
    table("A")(0) shouldEqual 10
    table("A")(1) shouldEqual 20
    table("A")(2) shouldEqual 30
    table("B")(0) shouldEqual 40
    table("B")(1) shouldEqual 50
    assertThrows[NoSuchElementException]{ table("B")(2) }
  }

  FromContent should "create an empty table if no cells are specified" in {
    Table.fromContent(Seq.empty) should be(empty)
  }
  it should "create a table with the specified entries" in {
    Table.fromContent(content = Seq(
      ("A", 0, 10), ("A", 1, 20), ("A", 2, 30),
      ("B", 0, 40), ("B", 1, 50),
    )) shouldEqual genericTable
  }

  Empty should "create an empty table" in {
    Table.empty should be(empty)
  }

  Content should "get the rows, column and cells of a table" in {
    genericTable.content should contain theSameElementsAs Set(
      ("A", 0, 10), ("A", 1, 20), ("A", 2, 30),
      ("B", 0, 40), ("B", 1, 50),
    )
  }

  Rows should "get the rows of a table" in {
    genericTable.rows should contain theSameElementsAs Set("A", "B")
  }

  Columns should "get the columns of a table" in {
    genericTable.columns should contain theSameElementsAs Set(0, 1, 2)
  }

  Cells should "get the cells of a table" in {
    genericTable.cells should contain theSameElementsAs Set(10, 20, 30, 40, 50)
  }

  CellAt should "get the value of a specific cell in a table if present" in {
    val table = genericTable
    table.cellAt("A", 0) shouldEqual Some(10)
    table.cellAt("A", 1) shouldEqual Some(20)
    table.cellAt("A", 2) shouldEqual Some(30)
    table.cellAt("B", 0) shouldEqual Some(40)
    table.cellAt("B", 1) shouldEqual Some(50)
    table.cellAt("B", 2) shouldEqual None
  }

  Transposed should "create the transposition of a mono-typed table" in {
    monotypedTable.transposed shouldEqual Table(
      0 -> Map(0 -> 10),
      1 -> Map(0 -> 20, 1 -> 50),
      2 -> Map(0 -> 30, 1 -> 60, 2 -> 90),
    )
  }
  it should "create the transposition of generic table" in {
    genericTable.transposed shouldEqual Table(
      0 -> Map("A" -> 10, "B" -> 40),
      1 -> Map("A" -> 20, "B" -> 50),
      2 -> Map("A" -> 30),
    )
  }

  WithTransposed should "merge a mono-typed table with its transposition" in {
    monotypedTable.withTransposed shouldEqual Table(
      0 -> Map(0 -> 10, 1 -> 20, 2 -> 30),
      1 -> Map(0 -> 20, 1 -> 50, 2 -> 60),
      2 -> Map(0 -> 30, 1 -> 60, 2 -> 90),
    )
  }
  it should "merge a generic table with its transposition" in {
    genericTable.withTransposed shouldEqual Table(
      0   -> Map(                           "A" -> 10, "B" -> 40),
      1   -> Map(                           "A" -> 20, "B" -> 50),
      2   -> Map(                           "A" -> 30),
      "A" -> Map(0 -> 10, 1 -> 20, 2 -> 30),
      "B" -> Map(0 -> 40, 1 -> 50),
    )
  }

/** Companion object of [[TableTest]]. */
object TableTest:
  /** A collection of table samples. */
  object Samples:
    /**
     * {{{
     *     | 0  | 1  | 2  |
     * ----|----|----|----|
     *  A  | 10 | 20 | 30 |
     *  B  | 40 | 50 |    |
     * }}}
     */
    def genericTable: Table[String, Int, Int] =
      Table.fromRows(
        Seq(
          "A" -> Map(0 -> 10, 1 -> 20, 2 -> 30),
          "B" -> Map(0 -> 40, 1 -> 50),
        )
      )

    /**
     * {{{
     *     | 0  | 1  | 2  |
     * ----|----|----|----|
     *  0  | 10 | 20 | 30 |
     *  1  |    | 50 | 60 |
     *  2  |    |    | 90 |
     * }}}
     */
    def monotypedTable: Table[Int, Int, Int] =
      Table.fromRows(
        Seq(
          0 -> Map(0 -> 10, 1 -> 20, 2 -> 30),
          1 -> Map(1 -> 50, 2 -> 60),
          2 -> Map(2 -> 90),
        )
      )
