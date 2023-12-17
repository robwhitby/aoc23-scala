object day11 {

  case class Point(x: Int, y: Int)

  case class Universe(input: List[String], expansionFactor: Int) {
    val galaxies: Seq[Point] = for {
      y <- input.indices
      x <- input.head.indices
      if input(y)(x) == '#'
    } yield Point(x, y)

    private val emptyYs = input.indices.filterNot(input(_).contains('#'))
    private val emptyXs = input.transpose.zipWithIndex.filterNot(_._1.contains('#')).map(_._2)

    def distance(p1: Point, p2: Point): Long = {
      def expand(i: Int, empties: Seq[Int]) = if (empties.contains(i)) expansionFactor else 1
      val xs = (p1.x.min(p2.x)+1 to p1.x.max(p2.x)).map(expand(_, emptyXs))
      val ys = (p1.y.min(p2.y)+1 to p1.y.max(p2.y)).map(expand(_, emptyYs))
      xs.sum + ys.sum
    }
  }

  def part1(input: List[String], expansionFactor: Int = 2): Long = {
    val u = Universe(input, expansionFactor)
    u.galaxies
      .combinations(2)
      .map{case Seq(a,b) => u.distance(a, b)}
      .sum
  }

  def part2(input: List[String], expansionFactor: Int): Long =
    part1(input, expansionFactor)
}
