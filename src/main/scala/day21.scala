import scala.annotation.tailrec

case class day21(input: List[String]) {
  val (width, height) = (input.head.length, input.length)
  val start = {
    val y = input.indexWhere(_.contains('S'))
    val x = input(y).indexOf('S')
    Point(x, y)
  }

  case class Point(x: Int, y: Int) {
    def neighbours(): Set[Point] =
      Set(Point(x-1, y), Point(x+1, y), Point(x, y-1), Point(x, y+1)).filter(inBounds)
  }

  def inBounds(p: Point) =
    val (mx, my) = (p.x % width, p.y % height)
    val x = if mx >= 0 then mx else mx + width
    val y = if my >= 0 then my else my + height
    input(y)(x) != '#'

  def reachable(steps: Int): Int =
    val modSteps = steps % 2
    @tailrec
    def rec(next: Set[Point] = Set(start), reached: Set[Point] = Set.empty, count: Int = 0): Int =
      if (count == steps) reached.size
      else
        val ps = next.flatMap(_.neighbours())
        rec(ps, if count % 2 != modSteps then reached ++ ps else reached, count+1)
    rec()

  def part1(steps: Int): Long = reachable(steps)

  def part2(steps: Int): Long = reachable(steps)
}