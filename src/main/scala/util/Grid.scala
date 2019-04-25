package util

case class Grid (size: Int) {
  val axis: Vector[Int] = (0 until size).toVector

  val coordinates: Vector[(Int, Int)] = axis.flatMap (r => axis.map (c => (r, c)))

  val directions = Vector ((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

  val neighbours: Map[(Int, Int), Vector[(Int, Int)]] =
    coordinates.map (c => (c, directions.flatMap (d => translate (c, d)))).toMap

  def translate (coordinate: (Int, Int), difference: (Int, Int)): Option[(Int, Int)] = {
    val translated = (coordinate._1 + difference._1, coordinate._2 + difference._2)
    if (validCoordinate (translated)) Some (translated) else None
  }

  def validCoordinate (coordinate: (Int, Int)) =
    coordinate._1 >= 0 && coordinate._2 >= 0 && coordinate._1 < size && coordinate._2 < size
}
