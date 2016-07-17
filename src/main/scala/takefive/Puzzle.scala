package takefive

class Puzzle (rows: Vector[Vector[Int]], columns: Vector[Vector[Int]]) {
  def row (index: Int): Vector[Int] =
    rows (index)

  def column (index: Int): Vector[Int] =
    columns (index)

  override def toString: String =
    rows.map (_.mkString ("[", ",", "]")).mkString (" ") + "\n" + columns.map (_.mkString ("[", ",", "]")).mkString (" ")
}
