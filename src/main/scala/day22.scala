import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

object day22 {

  case class Point(x: Int, y: Int, z: Int)

  case class Brick(a: Point, b: Point, label: Char) {
    val points = (for {x <- a.x to b.x; y <- a.y to b.y; z <- a.z to b.z} yield Point(x, y, z)).toSet

    def down() = Brick(a.copy(z = a.z-1), b.copy(z = b.z-1), label)
  }

  case class Stack(bricks: Vector[Brick] = Vector.empty, graph: Map[Char, Set[Char]] = Map.empty) {

    @tailrec
    private def fallBrick(brick: Brick): Stack =
      val below = bricksBelow(brick)
      if (brick.a.z > 1 && below.isEmpty) {
        fallBrick(brick.down())
      }
      else Stack(bricks.appended(brick), graph.updated(brick.label, below))

    def fall(): Stack = bricks.foldLeft(Stack()){ (s,b) => s.fallBrick(b) }

    def bricksBelow(brick: Brick): Set[Char] =
      val diff = brick.down().points diff brick.points
      bricks.filter(_.points.exists(diff.contains)).map(_.label).toSet

    def wouldFallWithout(without: Set[Char]): Set[Char] = without
      .flatMap(w => graph.filter((_,v) => v.contains(w)).keySet)
      .filter { graph(_).subsetOf(without) }

    @tailrec
    final def countFallers(without: Set[Char]): Long =
      val wouldFall = wouldFallWithout(without)
      if (wouldFall.subsetOf(without)) wouldFall.size
      else countFallers(without ++ wouldFall)
  }

  def parseInput(input: List[String]): Stack =
    val pattern = """(.+),(.+),(.+)~(.+),(.+),(.+)""".r
    val bricks = input.zipWithIndex.map{
      case (pattern(x1,y1,z1,x2,y2,z2), i) =>
        Brick(Point(x1.toInt,y1.toInt,z1.toInt), Point(x2.toInt,y2.toInt,z2.toInt), (i+65).toChar)
    }
    Stack(bricks.sortBy(_.a.z).toVector)

  def part1(input: List[String]): Long =
    val graph = parseInput(input).fall().graph
    graph
      .keys
      .count{ k => !graph.values.exists(_ == Set(k)) }

  def part2(input: List[String]): Long =
    val stack = parseInput(input).fall()
    stack
      .graph
      .keys
      .toSeq
      .par
      .map(k => stack.countFallers(Set(k)))
      .sum
}