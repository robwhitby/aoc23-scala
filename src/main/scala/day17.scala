import scala.annotation.tailrec

object day17 {

  case class Point(x: Int, y: Int)

  val North = Point(0,-1)
  val South = Point(0,1)
  val East = Point(1,0)
  val West = Point(-1,0)

  case class Position(point: Point, dir: Point, heat: Int = 0) {

    def next(grid: Grid): Seq[Position] = {

      def move(nextDir: Point): Seq[Position] = {
        val moves = (1 to grid.maxStraight).foldLeft(Seq(this)) { (acc, _) =>
          val last = acc.last
          val nextPoint = Point(last.point.x + nextDir.x, last.point.y + nextDir.y)
          if (grid.inBounds(nextPoint)) acc :+ Position(nextPoint, nextDir, last.heat + grid.heatAt(nextPoint))
          else acc
        }
        moves.drop(grid.minStraight)
      }

      if (point == grid.end) Seq.empty
      else {
        dir match {
          case North | South => move(East) ++ move(West)
          case East | West => move(North) ++ move(South)
        }
      }
    }

    def withoutHeat: Position = this.copy(heat = 0)
  }

  case class Grid(grid: List[String], minStraight: Int, maxStraight: Int) {
    private val start = Set(Position(Point(0, 0), East), Position(Point(0, 0), South))
    val end: Point = Point(grid.head.length - 1, grid.length - 1)

    def inBounds(p: Point): Boolean = grid.head.indices.contains(p.x) && grid.indices.contains(p.y)
    def heatAt(p: Point): Int = grid(p.y)(p.x).asDigit

    @tailrec
    final def walk(p: Set[Position] = start, visited: Map[Position, Int] = Map.empty): Int = {
      if (p.isEmpty) {
        visited.filter((k,_) => k.point == end).values.min
      }
      else if (visited.get(p.head.withoutHeat).exists(_ <= p.head.heat)) {
        walk(p.tail, visited)
      }
      else {
        walk(p.tail ++ p.head.next(this), visited.updated(p.head.withoutHeat, p.head.heat))
      }
    }
  }

  def part1(input: List[String]): Int = Grid(input, 1, 3).walk()

  def part2(input: List[String]): Int = Grid(input, 4, 10).walk()
}
