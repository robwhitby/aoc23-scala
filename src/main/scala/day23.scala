import scala.annotation.tailrec

object day23:

  case class Point(x: Int, y: Int):
    lazy val (n,e,s,w) = (Point(x,y-1), Point(x+1,y), Point(x,y+1), Point(x-1,y))
    lazy val neighbours = Set(n,e,s,w)

  case class Grid(grid: Map[Point, Char]):
    val start = grid.keys.minBy(_.y)
    val end = grid.keys.maxBy(_.y)

    def nextPoints(p: Point): Set[Point] =
      val ps = grid(p) match {
        case '.' => p.neighbours
        case '^' => Set(p.n)
        case '>' => Set(p.e)
        case 'v' => Set(p.s)
        case '<' => Set(p.w)
      }
      ps intersect grid.keySet

    @tailrec
    private def walk(p: Point, previous: Point, steps: Int = 1): Option[(Point, Int)] =
      val ps = nextPoints(p) - previous
      if (p == start || p == end) Some(p, steps)
      else if (ps.isEmpty) None
      else if (ps.size == 1) walk(ps.head, p, steps + 1)
      else Some((p, steps))

    val graph = grid
      .keySet
      .filter(p => p == start || nextPoints(p).size > 2)
      .foldLeft(Map[Point, Set[(Point, Int)]]()) { (m, p) =>
        m + (p -> nextPoints(p).flatMap(walk(_, p)))
      }

    def longestPath(from: Point = start, visited: Set[Point] = Set(start), steps: Int = 0): Int =
      if from == end then steps else
      graph(from)
        .filterNot { (p, c) => visited.contains(p) }
        .map { (p, c) => longestPath(p, visited + p, steps + c) }
        .maxOption
        .getOrElse(0)

  def parseInput(input: List[String], removeSlopes: Boolean = false): Grid =
    val points = for {
      y <- input.indices
      x <- input.head.indices
      if input(y)(x) != '#'
    } yield Point(x, y) -> (if removeSlopes then '.' else input(y)(x))
    Grid(points.toMap)

  def part1(input: List[String]): Int =
    parseInput(input).longestPath()

  def part2(input: List[String]): Int =
    parseInput(input, true).longestPath()