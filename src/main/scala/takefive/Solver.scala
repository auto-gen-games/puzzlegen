package takefive

import Constants._

object Solver {
  import grid._

  /** Returns true if the given cell value is known to be in a particular position in the given length of possibilities */
  def certain (value: Int, length: Vector[Set[Int]]): Boolean =
    length.exists (set => set.size == 1 && set.head == value)

  /** Returns true if the given value is impossible in a given cell as part of a length either because it must be
    * present in the cells before or after in the length, or if the value occurs in the order for the length and the
    * preceding value in the order cannot occur in the cells before or the succeeding value cannot occur after. */
  def impossibleInLength (value: Int, order: Vector[Int], before: Vector[Set[Int]], after: Vector[Set[Int]]): Boolean =
    if (certain (value, before) || certain (value, after)) true
    else if (!order.contains (value)) false
    else {
      val position = order.indexOf (value)
      (position > 0 && !before.exists (s => s.contains (order (position - 1)))) ||
        (position < 4 && !after.exists (s => s.contains (order (position + 1))))
    }

  /** Returns the possible alternative grid constraints by instantiating the first undecided cell to all its possibilities */
  def hypotheses (partial: Partial): Vector[Partial] =
    coordinates.find (c => partial (c._1, c._2).size > 1) match {
      case None => Vector[Partial] ()
      case Some (c) => partial.alternatives (c._1, c._2)
    }

  /** From the given partial grid constraints, remove all impossible values from the given cell and return
    * the revised constraints plus true if there was any change from the original constraints, or None if no
    * possibilities are left for the cell after removal */
  def reduceCell (row: Int, column: Int, state: Partial, puzzle: Puzzle): Option[(Partial, Boolean)] = {
    /** Returns true if the given value is impossible in the given position in either the row or the column */
    def impossible (value: Int): Boolean =
      impossibleInLength (value, puzzle.row (row), state.rowBefore (row, column), state.rowAfter (row, column)) ||
        impossibleInLength (value, puzzle.column (column), state.columnBefore (column, row), state.columnAfter (column, row))

    // Get the current constraints for the cell
    val initial = state (row, column)
    // Filter out impossible values
    val filtered = initial.filter (value => !impossible (value))
    // If none have been filtered, return the original constraints, marked false for no changes
    // If there are no possibilities left for the cell, return None
    // Otherwise return the new constraints, marked true for changed
    if (filtered.isEmpty) None
    else if (initial == filtered) Some ((state, false))
    else Some ((state.update (row, column, filtered), true))
  }

  /** Filter out all values known to be impossible given the current constraints from one iteration of all cells
    * and return the revised constraints, or None if any cell has no possibilities following filtering */
  def reduce (partial: Partial, puzzle: Puzzle): Option[Partial] =
    coordinates.foldLeft (Option ((partial, false))) {
      case (None, _) => None
      case (Some ((state, changed)), coordinate) =>
        reduceCell (coordinate._1, coordinate._2, state, puzzle).map (s => (s._1, s._2 || changed))
    } match {
      case Some ((state, changed)) if changed => reduce (state, puzzle)
      case Some ((state, changed)) if !changed => Some (state)
      case None => None
    }

  /** Return all solutions to the given puzzle */
  def solve (puzzle: Puzzle): Vector[Solution] = {
    /** Return all solutions to the puzzle given the partial grid constraints, startState */
    def solveFrom (startState: Partial): Vector[Solution] = {
      reduce (startState, puzzle) match {
        case None => Vector[Solution] ()
        case Some (partial) =>
          if (partial.solved)
            Vector (partial.toSolution)
          else
            hypotheses (partial).flatMap (solveFrom)
      }
    }

    solveFrom (new Partial ())
  }

}
