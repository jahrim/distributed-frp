package it.unibo.distributedfrp.simulation.environment

/** The environment where the devices are situated in a FRASP simulation. */
trait Environment:
  /** @return the number of devices in this [[Environment]]. */
  def nDevices: Int
  /**
   * @param device the specified identifier.
   * @return the position of the device with the specified identifier.
   */
  def position(device: Int): (Double, Double)
  /**
   * @param device the specified identifier.
   * @return the neighbors of the device with the specified identifier.
   */
  def neighbors(device: Int): Iterable[Int]

/** Companion object of [[Environment]]. */
object Environment:
  /** @return an [[Environment]] with a single device. */
  def singleNode: Environment = new Environment:
    override def nDevices: Int = 1
    override def position(device: Int): (Double, Double) = (0, 0)
    override def neighbors(device: Int): Iterable[Int] = Iterable.single(0)

  /**
   * @param cols the specified number of columns.
   * @param rows the specified number of rows.
   * @return a grid [[Environment]] with the specified dimensions:
   *         - each cell of the grid is occupied by one device;
   *         - non-diagonally adjacent cells are occupied by neighboring
   *           devices.
   */
  def manhattanGrid(cols: Int, rows: Int): Environment =
    grid(cols, rows, (col, row) =>
      for
        c <- -1 to 1
        r <- -1 to 1
        if math.abs(c) + math.abs(r) < 2
      yield (col + c, row + r)
    )

  /**
   * @param cols the specified number of columns.
   * @param rows the specified number of rows.
   * @return a grid [[Environment]] with the specified dimensions:
   *         - each cell of the grid is occupied by one device;
   *         - adjacent cells are occupied by neighboring devices.
   */
  def euclideanGrid(cols: Int, rows: Int): Environment =
    grid(cols, rows, (col, row) =>
      for
        c <- -1 to 1
        r <- -1 to 1
      yield (col + c, row + r)
    )

  private def grid(cols: Int, rows: Int, candidateNeighbors: (Int, Int) => Iterable[(Int, Int)]): Environment =
    new Environment:
      private def row(device: Int): Int = device / cols
      private def col(device: Int): Int = device % cols
      override def nDevices: Int = rows * cols
      override def position(device: Int): (Double, Double) = (col(device), row(device))
      override def neighbors(device: Int): Iterable[Int] =
        val deviceCol = col(device)
        val deviceRow = row(device)
        val candidates = candidateNeighbors(deviceCol, deviceRow)
        candidates
          .filter((c, r) => c >= 0 && r >= 0 && c < cols && r < rows)
          .map((c, r) => r * cols + c)
