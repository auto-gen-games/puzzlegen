package takefive

import java.awt.{Graphics2D, Color}
import java.awt.Color.{BLACK, WHITE}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import Constants._

object Output {
  def drawPuzzle (puzzle: Puzzle, file: File) = {
    val canvas = new BufferedImage (cellPixels * 14 + 1, cellPixels * 14 + 1, BufferedImage.TYPE_INT_RGB)
    val graphics = canvas.createGraphics
    import graphics.{dispose, setColor, fillRect, drawLine, drawString}

    def line (x1: Int, y1: Int, width: Int, height: Int, colour: Color) = {
      setColor (colour)
      drawLine (x1, y1, x1 + width, y1 + height)
    }

    setColor (WHITE)
    fillRect (0, 0, canvas.getWidth, canvas.getHeight)
    for (row <- positions; column <- positions) {
      line ((column + 5) * cellPixels, (row + 5) * cellPixels, cellPixels, 0, BLACK)
      line ((column + 5) * cellPixels, (row + 5) * cellPixels, 0, cellPixels, BLACK)
    }
    for (index <- positions; order <- 0 to 4) {
      drawString (puzzle.row (index)(order).toString, order * cellPixels, (index + 6) * cellPixels)
      drawString (puzzle.column (index)(order).toString, (index + 5) * cellPixels, (order + 1) * cellPixels)
    }
    line (5 * cellPixels, 14 * cellPixels, 9 * cellPixels, 0, BLACK)
    line (14 * cellPixels, 5 * cellPixels, 0, 9 * cellPixels, BLACK)

    dispose ()
    ImageIO.write (canvas, "png", file)
  }

}
