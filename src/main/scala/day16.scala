import scala.annotation.tailrec

object day16 {

  case class Dir(x: Int, y: Int)

  case class Point(x: Int = 0, y: Int = 0, d: Dir = Dir(1,0)) {
    def next(value: Char): Set[Point] = value match {
      case '/' => Set(move(Dir(d.y * -1, d.x * -1)))
      case '\\' => Set(move(Dir(d.y, d.x)))
      case '|' if d.y == 0 => Set(move(Dir(0,1)), move(Dir(0,-1)))
      case '-' if d.x == 0 => Set(move(Dir(1,0)), move(Dir(-1,0)))
      case _ => Set(move(d))
    }

    private def move(d: Dir) = Point(x + d.x, y + d.y, d)
  }

  case class Grid(grid: List[String]) {
    private def inBounds(p: Point) = grid.head.indices.contains(p.x) && grid.indices.contains(p.y)

    @tailrec
    final def walk(p: Set[Point] = Set(Point()), visited: Set[Point] = Set.empty): Set[Point] = {
      if (p.isEmpty) visited
      else if (!inBounds(p.head) || visited.contains(p.head)) walk(p.tail, visited)
      else walk(p.head.next(grid(p.head.y)(p.head.x)) ++ p.tail, visited + p.head)
    }

  }

  def part1(input: List[String]): Int = {
    Grid(input)
      .walk()
      .map(_.copy(d=Dir(0,0)))
      .size
  }

}
