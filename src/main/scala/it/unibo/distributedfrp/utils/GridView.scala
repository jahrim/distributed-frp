package it.unibo.distributedfrp.utils

import scala.annotation.tailrec

/** A wrapper that allows to display an [[Iterable]] of values as a grid. */
case class GridView private (elems: Iterable[_], rows: Int = 0, cols: Int = 0):
  private val (_rows, _cols): (Int, Int) = autoSize(rows, cols)
  private val _size: Int = this._rows * this._cols

  /**
   * Print a grid of the values of this [[GridView]] to the standard output.
   *
   * @param spacing the minimum number of spaces between each value horizontally. Defaults to 3.
   * @param header  true if an header with additional information should be displayed together
   *                with the values of this [[GridView]]; false otherwise. Defaults to true.
   */
  def show(spacing: Int = 3, header: Boolean = true): Unit =
    println(this.toString(spacing, header))

  override def toString: String = toString()

  /** @return the [[String]] that would be printed by [[show]] with the same parameters. */
  def toString(spacing: Int = 3, header: Boolean = true): String =
    val subgrid: Seq[_] = elems.toSeq.take(this._size)
    val maxStringLength: Int = subgrid.map(_.toString.length).maxOption.getOrElse(1)
    val gridString: Seq[String] = subgrid.map(elem => addLeftPadding(elem.toString, maxStringLength))
    val spacingString: String = " ".repeat(spacing)
    StringBuilder()
      .append(if header then s"GridView ${this._rows}x${this._cols}: " else "")
      .append("[")
      .append(
        gridString.foldLeft((0, ""))((acc, next) => (
          acc._1 + 1,
          acc._2 + (if acc._1 % this._cols == 0 then "\n" else "") + spacingString + next
        ))._2
      )
      .append(if elems.nonEmpty then "\n" else "")
      .append("]")
      .toString()

  /**
   * Evaluate the size of the smallest grid that fits the values of this [[GridView]],
   * taking the specified dimensions as set.
   *
   * @param rows the specified number of rows of the grid. Defaults to 0, meaning that it will
   *             be automatically evaluated.
   * @param cols the specified number of columns of the grid. Defaults to 0, meaning that it will
   *             be automatically evaluated.
   * @return the sizes of the smallest grid that can fit the values of this [[GridView]].
   * @note when automatically evaluating the size of the grid, the smallest grid which fits
   *       the values of this [[GridView]] will be chosen, giving priority to columns over rows,
   *       meaning that horizontal grids will be preferred over vertical grids.
   */
  @tailrec
  private def autoSize(rows: Double = 0, cols: Double = 0): (Int, Int) =
    (rows.ceil, cols.ceil) match
      case (rows, cols) if elems.isEmpty => (rows.toInt, cols.toInt)
      case (0, 0) => autoSize(cols = math.sqrt(elems.size))
      case (0, cols) => autoSize(elems.size / cols, cols)
      case (rows, 0) => autoSize(rows, elems.size / rows)
      case (rows, cols) => (rows.toInt, cols.toInt)

  /**
   * @param string       the specified [[String]].
   * @param targetLength the specified length.
   * @return a new [[String]] obtained by adding some white-spaces to the left
   *         of the specified [[String]] until it reaches the specified length.
   */
  private def addLeftPadding(string: String, targetLength: Int): String =
    " ".repeat(targetLength - string.length) + string

/** Companion object of [[GridView]]. */
object GridView:

  /**
   * A mapping function from elements to their index in a [[GridView]].
   *
   * @tparam A the type of elements.
   */
  @FunctionalInterface
  trait IndexIndexer[A] extends (A => Int)

  /**
   * A mapping function from elements to their coordinates in a [[GridView]].
   *
   * @tparam A the type of elements.
   */
  @FunctionalInterface
  trait CoordsIndexer[A] extends (A => (Int, Int)):
    /**
     * @param nColumns the specified number of columns.
     * @return the [[IndexIndexer]] equivalent to this [[CoordsIndexer]] within a
     *         grid with the specified number of columns.
     */
    def toIndexIndexer(nColumns: Int): IndexIndexer[A] =
      e => Some(this(e)).map((row, column) => row * nColumns + column).get

  /**
   * Create a new [[GridView]], displaying the specified values as a grid with the
   * specified number of rows and columns. After determining the size of the grid,
   * the [[GridView]] will scan the specified values in order, positioning them in
   * the first available cell in the grid.
   *
   * @param elems   the specified values.
   * @param rows    the number of rows of the grid. Defaults to 0, meaning that it will
   *                be automatically evaluated.
   * @param cols    the number of columns of the grid. Defaults to 0, meaning that it will
   *                be automatically evaluated.
   * @note when automatically evaluating the size of the grid, the smallest grid which fits
   *       the values of this [[GridView]] will be chosen, giving priority to columns over
   *       rows, meaning that horizontal grids will be preferred over vertical grids.
   */
  def loose(elems: Iterable[_], rows: Int = 0, cols: Int = 0): GridView = GridView(elems, rows, cols)

  /**
   * As [[loose]], but uses the specified indexer to set the coordinates of the specified
   * values inside the grid. Contrary to [[loose]], where values are always assigned the
   * first available position in the grid, [[fixedWithIndex]] allows the user to decide
   * where to position the specified values.
   *
   * The coordinates in the grid are interpreted as an index, starting from 0, increasing
   * left-to-right and top-to-bottom (i.e. reading order). In order to determine the position
   * in the grid corresponding to an index, it is required to specify the size of the grid.
   *
   * @param elems   the specified values.
   * @param rows    the number of rows of the grid.
   * @param columns the number of columns of the grid.
   * @param indexer a mapping function from values to their corresponding index in the grid.
   * @note values at negative coordinates will be ignored.
   */
  def fixedWithIndex[A](elems: Iterable[A], rows: Int, columns: Int)(using indexer: IndexIndexer[A]): GridView =
    val blankGrid: Map[Int, String] = Map.from(Seq.range(0, rows * columns).map(i => i -> "_"))
    val grid: Map[Int, String] = Map.from(elems.toSeq.map(e => indexer(e) -> e.toString).filter(_._1 >= 0))
    GridView.loose((blankGrid ++ grid).toSeq.sortBy(_._1).map(_._2), rows, columns)

  /**
   * As [[fixedWithIndex]], but the coordinates in the grid are interpreted as a pair of indexes,
   * starting from 0, indicating the row and column in the grid. Contrary to [[showWithIndex]],
   * it is not required to specify the size of the grid, which can be derived from the greatest
   * pair of indexes assigned to the specified values.
   *
   * @param elems   the specified values.
   * @param rows    the number of rows of the grid. Defaults to 0, meaning that it will
   *                be automatically evaluated as the greatest row assigned to the specified
   *                values.
   * @param columns the number of columns of the grid. Defaults to 0, meaning that it will
   *                be automatically evaluated as the greatest column assigned to the specified
   *                values.
   * @param indexer a mapping function from values to their corresponding indexes in the grid.
   * @note values at negative coordinates will be ignored.
   */
  def fixedWithCoords[A](elems: Iterable[A], rows: Int = 0, columns: Int = 0)(using indexer: CoordsIndexer[A]): GridView =
    val coords: Iterable[(Int, Int)] = Map.from(elems.toSeq.map(e => indexer(e) -> e)).keys
    val actualRows: Int = if rows > 0 then rows else coords.map(_._1).maxOption.map(_ + 1).getOrElse(0)
    val actualColumns: Int = if columns > 0 then columns else coords.map(_._2).maxOption.map(_ + 1).getOrElse(0)
    GridView.fixedWithIndex(elems, actualRows, actualColumns)(using indexer.toIndexIndexer(actualColumns))
