import scala.annotation.tailrec

object day14 {

  implicit class Grid(val g: List[String]) {
    def rotate90: List[String] = g.transpose.map(_.reverse.mkString)

    def tilt: List[String] = g.map(tiltLine)

    private def tiltLine(s: String): String = {
      if (!s.contains("O")) s
      else s.replace("#", "!#!")
        .split('!')
        .map(_.sorted)
        .mkString
    }

    def tiltNorth: List[String] = g.rotate90.tilt.rotate90.rotate90.rotate90

    def cycle: List[String] =
      g.rotate90.tilt
        .rotate90.tilt
        .rotate90.tilt
        .rotate90.tilt

    def totalLoad: Int =
      g.zipWithIndex.map{ (l, i) =>
        l.count(_=='O') * (g.length - i)
      }.sum
  }

  def part1(input: List[String]): Int = {
    input
      .tiltNorth
      .totalLoad
  }

  def part2(input: List[String]): Int = {

    @tailrec
    def findLoop(grid: List[String], history: Map[List[String], Int] = Map.empty, i: Int = 0): (List[String], Map[List[String], Int], Int) = {
      if (history.contains(grid)) (grid, history, i)
      else findLoop(grid.cycle, history + (grid -> i), i + 1)
    }

    val (grid, history, loopEnd) = findLoop(input)
    val loopStart = history(grid)
    val remainder = (1000000000 - loopEnd) % (loopEnd - loopStart)
    val finalGrid = history.find(_._2 == loopStart + remainder).get._1
    finalGrid.totalLoad
  }
}
