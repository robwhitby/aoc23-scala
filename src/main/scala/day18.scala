import scala.annotation.tailrec

object day18 {

  case class Point(x: Int, y: Int) {
    def add(p: Point): Point = Point(x + p.x, y + p.y)
  }

  object Point {
    def fromDir(d: Char): Point = d match {
      case 'U' => Point(0,-1)
      case 'D' => Point(0,1)
      case 'L' => Point(-1,0)
      case 'R' => Point(1,0)
    }
  }

  case class Grid(squares: Set[Point]) {
    val (xmin, xmax) = (squares.map(_.x).min, squares.map(_.x).max)
    val (ymin, ymax) = (squares.map(_.y).min, squares.map(_.y).max)

    def inBounds(p: Point): Boolean =
      p.x >= xmin && p.x <= xmax && p.y >= ymin && p.y <= ymax

    def neighbours(p: Point): Set[Point] =
      Set(p.add(Point(0, 1)), p.add(Point(0, -1)), p.add(Point(1, 0)), p.add(Point(-1, 0)))
        .filter(inBounds)

    def fill(): Grid = {
      @tailrec
      def rec(points: Set[Point] = Set(pointInside()), updated: Set[Point] = Set.empty): Set[Point] = {
        if (points.isEmpty) updated
        else {
          val toCheck = points.tail ++ neighbours(points.head).filter { p =>
            !squares.contains(p) && !updated.contains(p)
          }
          rec(toCheck, updated + points.head)
        }
      }
      Grid(squares ++ rec())
    }

    def pointInside(): Point = {
      squares
        .groupBy(_.y)
        .filter((_,ps) => ps.size == 2)
        .filter((_,ps) => ps.map(_.x).max - ps.map(_.x).min > 1)
        .map((y,ps) => Point(ps.map(_.x).min + 1, y))
        .head
    }
  }

  object Grid {
    def fromSteps(steps: List[Step]): Grid = {
      val ps = steps.foldLeft(List(Point(0,0))){ (ps, step) =>
        (1 to step.distance).foldLeft(ps) { (ps, _) =>
          ps :+ ps.last.add(step.dir)
        }
      }
      Grid(ps.toSet)
    }
  }

  case class Step(dir: Point, distance: Int, colour: String)

  object Step {
    def listFrom(input: List[String]): List[Step] = {
      val pattern = """(.) (.+) ..(.+).""".r
      input.map {
        case pattern(dir, dis, col) => Step(Point.fromDir(dir.head), dis.toInt, col)
      }
    }
  }

  def part1(input: List[String]): Int = {
    val steps = Step.listFrom(input)
    val grid = Grid.fromSteps(steps)
    val filled = grid.fill()
    filled.squares.size
  }

}
