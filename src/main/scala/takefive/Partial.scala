package takefive

import Constants._

class Partial (options: Vector[Vector[Set[Int]]]) {
  def this () = this (Vector.fill (9)(Vector.fill (9)(cellPossibilities)))

  val solved: Boolean =
    options.forall (_.forall (_.size == 1))

  private val transposedOptions: Vector[Vector[Set[Int]]] =
    options.transpose

  def apply (row: Int, column: Int): Set[Int] =
    options (row)(column)

  def alternatives (row: Int, column: Int): Vector[Partial] =
    options (row)(column).toVector.map (value => update (row, column, Set (value)))

  def row (index: Int): Vector[Set[Int]] =
    options (index)

  def rowBefore (index: Int, beforeColumn: Int): Vector[Set[Int]] =
    row (index).take (beforeColumn)

  def rowAfter (index: Int, afterColumn: Int): Vector[Set[Int]] =
    row (index).takeRight (8 - afterColumn)

  def column (index: Int): Vector[Set[Int]] =
    transposedOptions (index)

  def columnBefore (index: Int, beforeRow: Int): Vector[Set[Int]] =
    column (index).take (beforeRow)

  def columnAfter (index: Int, afterRow: Int): Vector[Set[Int]] =
    column (index).takeRight (8 - afterRow)

  def toSolution: Solution =
    options.map (_.map (_.head))

  def update (row: Int, column: Int, newOptions: Set[Int]): Partial =
    new Partial (options.updated (row, options (row).updated (column, newOptions)))
}
