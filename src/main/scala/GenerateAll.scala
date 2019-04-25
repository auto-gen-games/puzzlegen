import java.io.File
import scala.util.Random

object GenerateAll extends App {
  val puzzleType = Generaters.starsPuzzles
  val seedStart = 3l
  val puzzleCount = 10

  val directory = new File (puzzleType.name)
  directory.mkdirs ()
  for (count <- 0 until puzzleCount) {
    val puzzleSolution = puzzleType.generate (new Random (seedStart + count))
    puzzleType.drawPuzzle (puzzleSolution._1, new File (directory, "puzzle" + (seedStart + count) + ".png"))
    puzzleType.drawSolution (puzzleSolution._1, puzzleSolution._2, new File (directory, "solution" + (seedStart + count) + ".png"))
  }
}
