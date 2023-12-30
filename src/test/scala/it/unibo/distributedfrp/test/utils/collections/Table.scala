package it.unibo.distributedfrp.test.utils.collections

/** A utility object for creating tables of values. */
object Table:
  /** A table of values with generic indexes for accessing its rows and columns. */
  type Table[R, C, +V] = Map[R, Map[C, V]]

  /**
   * @tparam R the type of indexes used for accessing the rows of the [[Table Table]].
   * @tparam C the type of indexes used for accessing the columns of the [[Table Table]].
   * @tparam V the type of values stored in the [[Table Table]].
   * @return an empty [[Table Table]].
   */
  def empty[R, C, V]: Table[R, C, V] = Table.fromRows(Seq.empty)

  /** As [[fromRows fromRows(rows)]]. */
  def apply[R, C, V](rows: (R, Map[C, V])*): Table[R, C, V] = Table.fromRows(rows)

  /**
   * @param rows the specified rows.
   * @tparam R the type of indexes used for accessing the rows of the [[Table Table]].
   * @tparam C the type of indexes used for accessing the columns of the [[Table Table]].
   * @tparam V the type of values stored in the [[Table Table]].
   * @return a new [[Table Table]] containing the specified rows.
   */
  def fromRows[R, C, V](rows: IterableOnce[(R, Map[C, V])]): Table[R, C, V] = Map.from(rows)

  /**
   * @param content the specified content, as a sequence of values bound to their
   *                corresponding rows and columns.
   * @tparam R the type of indexes used for accessing the rows of the [[Table Table]].
   * @tparam C the type of indexes used for accessing the columns of the [[Table Table]].
   * @tparam V the type of values stored in the [[Table Table]].
   * @return a new [[Table Table]] containing the specified entries.
   */
  def fromContent[R, C, V](content: Iterable[(R, C, V)]): Table[R, C, V] =
    content.groupBy(_._1).map((row, rcvs) => row -> Map.from(rcvs.map(rcv => rcv._2 -> rcv._3)))

  extension[R, C, V] (self: Table[R, C, V]) {
    /**
     * @return an [[Iterable]] over the [[rows]], [[columns]] and
     *         [[cells]] of this [[Table Table]].
     */
    def content: Iterable[(R, C, V)] =
      self.rows.flatMap(row =>
        self.columns.map(column =>
          self.cellAt(row, column).map(cell => (row, column, cell))
        )
      ).collect { case Some(x) => x }
    /** @return the rows of this [[Table Table]]. */
    def rows: Iterable[R] = self.keys
    /** @return the columns of this [[Table Table]]. */
    def columns: Iterable[C] = self.values.foldLeft(Set.empty[C])(_ ++ _.keys)
    /** @return the values of this [[Table Table]]. */
    def cells: Iterable[V] = self.values.foldLeft(Seq.empty[V])(_ ++ _.values)
    /**
     * @param row    the specified row.
     * @param column the specified column.
     * @return an [[Option]] containing the value of the cell at the specified
     *         row and column, if present; an empty [[Option]] otherwise.
     */
    def cellAt(row: R, column: C): Option[V] = self.get(row).flatMap(_.get(column))
    /**
     * @return the symmetry of this [[Table Table]] with respect to the
     *         main diagonal of the table.
     */
    def transposed: Table[C, R, V] =
      Table.fromContent(self.content.map((row, column, cell) => (column, row, cell)))
    /**
     * @return a new [[Table Table]] obtained by updating the missing
     *         values of this [[Table Table]] with the corresponding
     *         values of its [[transposed transposition]].
     */
    def withTransposed: Table[R | C, R | C, V] =
      val generalizedSelf: Table[R | C, R | C, V] = self.asInstanceOf[Table[R | C, R | C, V]]
      Table.fromContent(generalizedSelf.transposed.content ++ generalizedSelf.content)
  }
