package stars

import scala.util.Random
import stars.Constants.{grid, stars}

object Generate extends App {
  import grid._

  def generate (chooser: Random): (Puzzle, Solution) = {
    def nonAdjacent (ys: Iterable[Int]): Vector[Int] =
      axis.filter (y => !ys.exists (previous => (previous - y).abs <= 1))

    def choose[T] (list: Seq[T]): Option[T] =
      if (list.isEmpty) None else Some (list (chooser.nextInt (list.size)))

    def generateRowStars (previousRowStars: Iterable[Int]): Vector[Int] = {
      def generateAll (remaining: Int, exclude: Vector[Int], generated: Vector[Int]): Option[Vector[Int]] =
        if (remaining == 0) Some (generated)
        else {
          choose (nonAdjacent (exclude)) match {
            case None => None
            case Some (next) => generateAll (remaining - 1, next +: exclude, next +: generated) match {
              case r@Some (result) => r
              case None => generateAll (remaining, exclude, generated)
            }
          }
        }
      generateAll (stars, previousRowStars.toVector, Vector [Int]()).get
    }

    def generateGridStars: Vector[(Int, Int)] = {
      def generateFollowing (row: Int, previousRowStars: Iterable[Int]): Vector[(Int, Int)] =
        if (row == size) Vector [(Int, Int)]()
        else {
          val next = generateRowStars (previousRowStars)
          next.map (column => (row, column)) ++ generateFollowing (row + 1, next)
        }
      generateFollowing (0, Nil)
    }

    def expand (region: Region, regions: RegionSet, checkBlocking: Boolean): Option[RegionSet] = {
      def expandFrom (options: Vector[(Int, Int)]): Option[RegionSet] =
        if (options.isEmpty) None
        else
          (regions.allocation (options.head) match {
            case None =>
              Some (regions.grow (region, options.head))
            case Some (bordering) =>
              if (region.numberOfStars + bordering.numberOfStars <= stars)
                Some (regions.merge (region, bordering))
              else
                None
          }) match {
            case Some (newRegions) if !checkBlocking || newRegions.regions.forall (other => escapable (other, newRegions)) =>
              Some (newRegions)
            case _ =>
              expandFrom (options.tail)
          }

      expandFrom (chooser.shuffle (region.adjacentTo))
    }

    def escapable (region: Region, regions: RegionSet): Boolean =
      if (region.numberOfStars == stars) true
      else
        expand (region, regions, checkBlocking = false) match {
          case None => false
          case Some (expanded) => escapable (expanded.superceding (region), expanded)
        }

    def allAllocated (regions: RegionSet): Boolean =
      coordinates.forall (c => regions.allocation (c).isDefined)

    def completed (regions: RegionSet): Boolean =
      allAllocated (regions) && regions.regions.forall (_.numberOfStars == stars)

    def expandToCompletion (regions: RegionSet): Option[RegionSet] = {
      def expandFrom (regionList: Vector[Region]): Option[RegionSet] =
        if (regionList.isEmpty) None
        else
          expand (regionList.head, regions, checkBlocking = true) match {
            case None => expandFrom (regionList.tail)
            case expanded@Some (_) => expanded
          }

      expandFrom (chooser.shuffle (regions.regions)) match {
        case None => None
        case Some (newRegions) =>
          if (completed (newRegions))
            Some (newRegions)
          else
            expandToCompletion (newRegions)
      }
    }

    def generatePuzzle: Puzzle = {
      val starMap = StarMap (size, generateGridStars)
      val startRegions = RegionSet (starMap.starPositions.map (p => new Region (p, starMap)))
      expandToCompletion (startRegions) match {
        case None => generatePuzzle
        case Some (finalRegions) => new Puzzle (finalRegions)
      }
    }

    val puzzle = generatePuzzle
    val solutions = Solve.solutions (puzzle, stopAt2 = true)
    if (solutions.size == 1) (puzzle, solutions.head.map (_.map (_ == StarCell)))
    else {
      print ("."); generate (chooser)
    }
  }

  /*  def generatePuzzles (number: Int): Vector[(Puzzle, Partial)] = {
      def generateMorePuzzles (generated: Vector[(Puzzle, Partial)]): Vector[(Puzzle, Partial)] = {
        println ("Generated: " + generated.size)
        if (generated.size >= number) generated else generateMorePuzzles (generateUntilValid +: generated)
      }
      generateMorePuzzles (Vector[(Puzzle, Partial)] ())
    }*/
}
