package object stars {
  type Partial = Vector[Vector[CellSolutionState]]
  type Hypothesis = (Int, Int, CellHypothesis)
  type Solution = Vector[Vector[Boolean]]
}
