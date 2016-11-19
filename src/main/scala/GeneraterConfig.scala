import java.io.File
import scala.util.Random

case class GeneraterConfig[Puzzle, Solution] (generate: Random => (Puzzle, Solution),
                                              drawPuzzle: (Puzzle, File) => Boolean,
                                              drawSolution: (Puzzle, Solution, File) => Boolean,
                                              name: String)
