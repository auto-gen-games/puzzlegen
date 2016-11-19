object Generaters {
  val starsPuzzles = new GeneraterConfig[stars.Puzzle, stars.Solution] (
    stars.Generate.generate, stars.Output.drawPuzzle, stars.Output.drawSolution, "stars")
  val takeFivePuzzles = new GeneraterConfig[takefive.Puzzle, takefive.Solution] (
    takefive.Generater.generate, takefive.Output.drawPuzzle, takefive.Output.drawSolution, "takefive")
}
