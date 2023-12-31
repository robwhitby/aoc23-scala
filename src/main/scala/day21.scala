object day21 {

  case class Point(x: Int, y: Int) {
    def neighbours(grid: Set[Point]): Set[Point] = {
      grid intersect Set(Point(x-1, y), Point(x+1, y), Point(x, y-1), Point(x, y+1))
    }
  }

  def parseInput(input: List[String]): (Set[Point], Point) = {
    val points = for {
      y <- input.indices
      x <- input.head.indices
      if input(y)(x) != '#'
    } yield Point(x,y)
    (points.toSet, points.find(p => input(p.y)(p.x) == 'S').get)
  }

  def part1(input: List[String], steps: Int): Long = {
    val (grid, start) = parseInput(input)
    (1 to steps).foldLeft(Set(start))((ps,_) => ps.flatMap(_.neighbours(grid))).size
  }

  def part2(input: List[String], steps: Int): Long = {
    val (grid, start) = parseInput(input)
    -1
  }
}