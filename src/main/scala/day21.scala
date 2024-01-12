case class day21(input: List[String]) {
  val (width, height) = (input.head.length, input.length)
  val start = {
    val y = input.indexWhere(_.contains('S'))
    val x = input(y).indexOf('S')
    Point(x, y)
  }

  case class Point(x: Int, y: Int) {
    def neighbours(): Set[Point] =
      Set(Point(x-1, y), Point(x+1, y), Point(x, y-1), Point(x, y+1))
        .filter(inBounds)
  }

  def inBounds(p: Point) =
    p.x >= 0 && p.x < width && p.y >= 0 && p.y < height && input(p.y)(p.x) != '#'

  def reachable(steps: Int) =
    (1 to steps).foldLeft(Set(start))((ps,_) => ps.flatMap(_.neighbours())).size

  def part1(steps: Int): Long = reachable(steps)

  def part2(steps: Int): Long = -1
}