package util

import java.awt.Color.WHITE
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}
import java.io.File
import javax.imageio.ImageIO

object GridOutput {
  val cellPixels = 10

  def draw (grid: Grid, maxRowLabels: Int, maxColumnLabels: Int,
            cellTop: (Int, Int) => Color, cellLeft: (Int, Int) => Color, gridBottom: Color, gridRight: Color,
            drawCell: ((Int, Int), (Int, Int), Int, Graphics2D) => Unit,
            drawLabel: (Int, Boolean, Int, (Int, Int), Int, Graphics2D) => Unit,
            file: File): Boolean = {
    import grid._

    val canvas = new BufferedImage (cellPixels * (9 + maxColumnLabels) + 1, cellPixels * (9 + maxRowLabels) + 1, BufferedImage.TYPE_INT_RGB)
    val graphics = canvas.createGraphics
    import graphics.{dispose, drawLine, fillRect, setColor}

    def line (x1: Int, y1: Int, width: Int, height: Int, colour: Color) = {
      setColor (colour)
      drawLine (x1, y1, x1 + width, y1 + height)
    }

    setColor (WHITE)
    fillRect (0, 0, canvas.getWidth, canvas.getHeight)
    for (row <- axis; column <- axis) {
      line ((column + maxRowLabels) * cellPixels, (row + maxColumnLabels) * cellPixels, cellPixels, 0, cellTop (row, column))
      line ((column + maxRowLabels) * cellPixels, (row + maxColumnLabels) * cellPixels, 0, cellPixels, cellLeft (row, column))
      drawCell ((row, column), ((column + maxRowLabels) * cellPixels, (row + maxColumnLabels) * cellPixels), cellPixels, graphics)
    }
    for (index <- axis; order <- 0 to maxRowLabels)
      drawLabel (index, true, order, (order * cellPixels, (index + maxColumnLabels) * cellPixels), cellPixels, graphics)
    for (index <- axis; order <- 0 to maxColumnLabels)
      drawLabel (index, false, order, ((index + maxRowLabels) * cellPixels, order * cellPixels), cellPixels, graphics)

    line (maxRowLabels * cellPixels, (size + maxColumnLabels) * cellPixels, size * cellPixels, 0, gridBottom)
    line ((size + maxRowLabels) * cellPixels, maxColumnLabels * cellPixels, 0, size * cellPixels, gridRight)

    dispose ()
    ImageIO.write (canvas, "png", file)
  }
}
