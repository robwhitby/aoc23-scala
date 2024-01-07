object day24:

  case class Point(x: BigDecimal, y: BigDecimal, z: BigDecimal)

  case class Line(p: Point, v: Point)

  def parseInput(input: List[String]) = input.map { line =>
    val c = line.split(",@".toCharArray).map(_.trim).map(BigDecimal(_))
    Line(Point(c(0), c(1), c(2)), Point(c(3), c(4), c(5)))
  }

  def intersection(line1: Line, line2: Line): Option[Point] =
    // y = ax + b
    val a1 = line1.v.y / line1.v.x
    val a2 = line2.v.y / line2.v.x

    // parallel
    if (a1 == a2) return None

    // a1x + b1 = a2x + b2
    // x(a1 - a2) = (b2 - b1)
    // x = (b2 - b1)/(a1 - a2)
    val b1 = line1.p.y - (a1 * line1.p.x)
    val b2 = line2.p.y - (a2 * line2.p.x)
    val x = (b2 - b1) / (a1 - a2)
    val y = a1 * x + b1

    val int = Point(x, y, 0)
    if (inFuture(line1, int) && inFuture(line2, int)) Some(int) else None

  def inFuture(l: Line, p: Point) =
    Seq(p.x - l.p.x, l.v.x).count(_ > 0) != 1 && Seq(p.y - l.p.y, l.v.y).count(_ > 0) != 1

  def part1(input: List[String], bounds: (BigDecimal, BigDecimal)): Int =
    def inBounds(p: Point) =
      p.x >= bounds._1 && p.x <= bounds._2 && p.y >= bounds._1 && p.y <= bounds._2

    parseInput(input)
      .combinations(2)
      .flatMap{ case Seq(a,b) => intersection(a,b) }
      .count(inBounds)

  def part2(input: List[String]): Int =
    // rock = (a,b)
    // position where (a,b) intersects (p,v)
    // a + tb = p + tv
    // a = p + t(v-b)
    ???