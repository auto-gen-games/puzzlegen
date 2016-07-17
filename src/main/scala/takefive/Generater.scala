package takefive

import takefive.Constants._
import takefive.Solver.solve
import scala.util.Random.{nextInt, shuffle}

object Generater {
  def generate: Puzzle = {
    def generateSolution: Solution = {
      def generateSolutionFrom (precedingRows: Vector[Vector[Int]]): Option[Solution] = {
        val transposed = precedingRows.transpose
        val unfilteredOptions = Vector.fill (9)(shuffle (cellPossibilities.toVector))
        val columnOptions =
          if (precedingRows.nonEmpty)
            positions.map (col => unfilteredOptions (col).filter (v => !transposed (col).contains (v)))
          else
            positions.map (unfilteredOptions)

        def generateRowFrom (precedingCells: Vector[Int]): Option[Vector[Int]] = {
          def generateFromCell (cellOptions: Vector[Int]): Option[Vector[Int]] =
            if (cellOptions.isEmpty) None
            else generateRowFrom (precedingCells :+ cellOptions.head).orElse (generateFromCell (cellOptions.tail))

          if (precedingCells.size == 9) Some (precedingCells)
          else generateFromCell (columnOptions (precedingCells.size).diff (precedingCells))
        }

        if (precedingRows.size == 9) Some (precedingRows)
        else generateRowFrom (Vector [Int]()).flatMap (newRow => generateSolutionFrom (precedingRows :+ newRow))
      }

      generateSolutionFrom (Vector[Vector[Int]] ()).getOrElse (generateSolution)
    }
    def generateValidPuzzle (expected: Solution): Puzzle = {
      def generatePuzzle: Puzzle = {
        def reduceToFive (values: Vector[Int]): Vector[Int] =
          if (values.size == 5) values else reduceToFive (values.patch (nextInt (values.size), Nil, 1))
        new Puzzle (expected.map (reduceToFive), expected.transpose.map (reduceToFive))
      }
      val puzzle = generatePuzzle
      print (".")
      val solutions = solve (puzzle)
      if (solutions.size == 1) puzzle else generateValidPuzzle (expected)
    }

    generateValidPuzzle (generateSolution)
  }
}
