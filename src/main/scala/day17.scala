import scala.annotation.tailrec

object day17 {

  case class Point(x: Int, y: Int)
  val North = Point(0,-1)
  val South = Point(0,1)
  val East = Point(1,0)
  val West = Point(-1,0)

  case class Position(point: Point, dir: Point, heat: Int = 0, straight: Int = 1) {
    def next(grid: Grid): Seq[Position] = {

      def move(nextDir: Point) = {
        val nextPoint = Point(point.x + nextDir.x, point.y + nextDir.y)
        val nextStraight = if (nextDir == dir) straight+1 else 1
        if (nextStraight < 4 && grid.inBounds(nextPoint)) {
          Some(Position(nextPoint, nextDir, heat + grid.heatAt(nextPoint), nextStraight))
        }
        else None
      }

      if (point == grid.end) Seq.empty
      else {
        val ps = dir match {
          case North | South => Seq(move(East), move(West))
          case East | West => Seq(move(North), move(South))
        }
        (ps :+ move(dir)).flatten
      }
    }

    def withoutHeat: Position = this.copy(heat = 0)
  }

  case class Grid(grid: List[String]) {
    def inBounds(p: Point): Boolean = grid.head.indices.contains(p.x) && grid.indices.contains(p.y)

    val end: Point = Point(grid.head.length - 1, grid.length - 1)

    def heatAt(p: Point): Int = grid(p.y)(p.x).asDigit

    @tailrec
    final def walk(p: Set[Position], visited: Map[Position, Int] = Map.empty): Option[Int] = {
      if (p.isEmpty) visited.filter(_._1.point == end).values.minOption
      else {
        val keys = (1 to p.head.straight).map(s => p.head.withoutHeat.copy(straight = s))
        if (keys.flatMap(visited.get).minOption.exists(_ <= p.head.heat)) {
          walk(p.tail, visited)
        }
        else {
          walk(p.tail ++ p.head.next(this), visited.updated(p.head.withoutHeat, p.head.heat))
        }
      }
    }
  }


  def part1(input: List[String]): Int = {
    Grid(input)
      .walk(Set(Position(Point(0, 0), East), Position(Point(0, 0), South)))
      .get
  }

}
