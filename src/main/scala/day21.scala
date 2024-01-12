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

  def reachable(steps: Long): Long =
    val modSteps = steps % 2
    @tailrec
    def rec(next: Set[Point] = Set(start), reached: Set[Point] = Set.empty, count: Int = 0): Int =
      if (count == steps) reached.size
      else
        val ps = next.flatMap(_.neighbours()).diff(reached)
        rec(ps, if count % 2 != modSteps then reached ++ ps else reached, count+1)
    rec()

  def part1(steps: Int): Long = reachable(steps)

  // f(rem + n*2w) = steps
  // f(rem), f(rem + 2w), f(rem + 4w), ...
  //
  // r = f(0) = c
  // s = f(1) = a + b + c
  // t = f(2) = 4a + 2b + c

  // c = r
  // b = s - c - a
  // a = (t - c)/2 - s + c
  def part2(steps: Long): Long =
    val rem = steps % (width*2)
    val (r, s, t) = (reachable(rem), reachable(rem + width*2), reachable(rem + width*4))
    val c = r
    val a = (t - c)/2 - s + c
    val b = s - c - a
    def f(x: Long) = a*x*x + b*x + c

    f(steps / (width * 2))
}