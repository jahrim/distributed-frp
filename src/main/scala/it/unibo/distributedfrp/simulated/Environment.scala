package it.unibo.distributedfrp.simulated

trait Environment:
  def nDevices: Int
  def position(device: Int): (Double, Double)
  def neighbors(device: Int): Iterable[Int]

object Environment:
  def grid(rows: Int, cols: Int): Environment = new Environment:
    private def row(device: Int): Int = device / cols

    private def col(device: Int): Int = device % cols

    override def nDevices: Int = rows * cols

    override def position(device: Int): (Double, Double) = (col(device), row(device))

    override def neighbors(device: Int): Iterable[Int] =
      val deviceCol = col(device)
      val deviceRow = row(device)
      val candidates = Seq(
        (deviceCol, deviceRow),
        (deviceCol + 1, deviceRow),
        (deviceCol - 1, deviceRow),
        (deviceCol, deviceRow + 1),
        (deviceCol, deviceRow - 1),
      )
      candidates.filter((c, r) => c >= 0 && r >= 0 && c < cols && r < rows).map((c, r) => r * cols + c)
