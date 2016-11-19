package takefive

object Constants {
  val cellPossibilities: Set[Int] = (1 to 9).toSet
  val positions: Vector[Int] = (0 to 8).toVector
  val coordinates: Vector[(Int, Int)] = positions.flatMap (row => positions.map (column => (row, column)))
  val cellPixels: Int = 10
}
