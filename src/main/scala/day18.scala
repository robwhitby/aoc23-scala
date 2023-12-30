import java.lang.Integer.parseInt

object day18 {

  case class Point(x: Int, y: Int) {
    def add(s: Step) = Point(x + s.dir.x * s.distance, y + s.dir.y * s.distance)
  }

  object Point {
    def fromDir(d: Char) = d match {
      case 'U' | '3'  => Point(0,-1)
      case 'D' | '1' => Point(0,1)
      case 'L' | '2' => Point(-1,0)
      case 'R' | '0' => Point(1,0)
    }
  }

  case class Step(dir: Point, distance: Int)

  def stepsToPoints(steps: Seq[Step]): Seq[Point] = {
    steps.foldLeft(Seq(Point(0, 0)))((ps, step) => ps :+ ps.last.add(step))
  }

  def area(steps: Seq[Step]): BigDecimal = {
    val points = stepsToPoints(steps)

    // https://en.wikipedia.org/wiki/Shoelace_formula
    val internalArea = points.sliding(2).map {
      case Seq(a, b) => BigDecimal(a.x) * b.y - BigDecimal(b.x) * a.y
    }.sum / 2

    // https://en.wikipedia.org/wiki/Pick%27s_theorem
    internalArea + (steps.map(s => BigDecimal(s.distance)).sum/2) + 1
  }

  def part1(input: List[String]): BigDecimal = {
    val pattern = """(.) (.+) .+""".r
    val steps = input.map {
      case pattern(dir, dis) => Step(Point.fromDir(dir.head), dis.toInt)
    }
    area(steps)
  }

  def part2(input: List[String]): BigDecimal = {
    val pattern = """. .+ ..(.....)(.).""".r
    val steps = input.map {
      case pattern(dis, dir) => Step(Point.fromDir(dir.head), parseInt(dis, 16))
    }
    area(steps)
  }
}
