import java.io.File

import takefive.{Generater => TakeFiveGenerater, Output => TakeFiveOutput}

object GenerateAll extends App {
  val takeFiveDir = new File ("takefive")
  val takeFiveCount = 10

  takeFiveDir.mkdirs ()
  for (count <- 1 to takeFiveCount)
    TakeFiveOutput.drawPuzzle (TakeFiveGenerater.generate, new File (takeFiveDir, "puzzle" + count + ".png"))
}
