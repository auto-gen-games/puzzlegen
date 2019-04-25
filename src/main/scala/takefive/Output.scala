package takefive

import java.awt.Color
import java.awt.Color.{BLACK, WHITE}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import takefive.Constants._

object Output {
  import grid._

  def drawPuzzle (puzzle: Puzzle, file: File): Boolean = {
    val canvas = new BufferedImage (cellPixels * 14 + 1, cellPixels * 14 + 1, BufferedImage.TYPE_INT_RGB)
    val graphics = canvas.createGraphics
    import graphics.{dispose, drawLine, drawString, fillRect, setColor}

    def line (x1: Int, y1: Int, width: Int, height: Int, colour: Color) = {
      setColor (colour)
      drawLine (x1, y1, x1 + width, y1 + height)
    }

    setColor (WHITE)
    fillRect (0, 0, canvas.getWidth, canvas.getHeight)
    for (row <- axis; column <- axis) {
      line ((column + 5) * cellPixels, (row + 5) * cellPixels, cellPixels, 0, BLACK)
      line ((column + 5) * cellPixels, (row + 5) * cellPixels, 0, cellPixels, BLACK)
    }
    for (index <- axis; order <- 0 to 4) {
      drawString (puzzle.row (index)(order).toString, order * cellPixels, (index + 6) * cellPixels)
      drawString (puzzle.column (index)(order).toString, (index + 5) * cellPixels, (order + 1) * cellPixels)
    }
    line (5 * cellPixels, 14 * cellPixels, 9 * cellPixels, 0, BLACK)
    line (14 * cellPixels, 5 * cellPixels, 0, 9 * cellPixels, BLACK)

    dispose ()
    ImageIO.write (canvas, "png", file)
  }

  def drawSolution (puzzle: Puzzle, solution: Solution, file: File): Boolean = {
    val canvas = new BufferedImage (cellPixels * 9 + 1, cellPixels * 9 + 1, BufferedImage.TYPE_INT_RGB)
    val graphics = canvas.createGraphics
    import graphics.{dispose, drawLine, drawString, fillRect, setColor}

    def line (x1: Int, y1: Int, width: Int, height: Int, colour: Color) = {
      setColor (colour)
      drawLine (x1, y1, x1 + width, y1 + height)
    }

    setColor (WHITE)
    fillRect (0, 0, canvas.getWidth, canvas.getHeight)
    setColor (BLACK)
    for (row <- axis; column <- axis) {
//      line (column * cellPixels, row * cellPixels, cellPixels, 0, BLACK)
//      line (column * cellPixels, row * cellPixels, 0, cellPixels, BLACK)
      drawString (solution (row)(column).toString, column * cellPixels, (row + 1) * cellPixels)
    }
//    line (0, 9 * cellPixels, 9 * cellPixels, 0, BLACK)
//    line (9 * cellPixels, 0, 0, 9 * cellPixels, BLACK)

    dispose ()
    ImageIO.write (canvas, "png", file)
  }

}
