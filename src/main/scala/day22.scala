import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

object day22 {

  case class Point(x: Int, y: Int, z: Int)

  case class Brick(a: Point, b: Point, label: Char) {
    val points = (for {x <- a.x to b.x; y <- a.y to b.y; z <- a.z to b.z} yield Point(x, y, z)).toSet

    def down() = Brick(a.copy(z = a.z-1), b.copy(z = b.z-1), label)

    def up() = Brick(a.copy(z = a.z+1), b.copy(z = b.z+1), label)
  }

  case class Stack(bricks: Vector[Brick] = Vector.empty) {

    @tailrec
    private def fallBrick(stack: Stack, brick: Brick): Stack =
      if (brick.a.z > 1 && stack.bricksBelow(brick).isEmpty) {
        fallBrick(stack, brick.down())
      }
      else Stack(stack.bricks.appended(brick))

    def fall(): Stack = bricks.foldLeft(Stack())(fallBrick)

    def bricksAbove(brick: Brick): Set[Brick] = overlap(brick, brick.up())

    def bricksBelow(brick: Brick): Set[Brick] = overlap(brick, brick.down())

    def overlap(from: Brick, to: Brick): Set[Brick] =
      val diff = to.points diff from.points
      bricks.filter(_.points.exists(diff.contains)).toSet

    def wouldFallWithout(bs: Set[Brick]): Set[Brick] = bs
      .flatMap(bricksAbove)
      .filter{ bricksBelow(_).forall(bs.contains) }

    def memoize[K, V](fn: K => V): K => V =
      val cache = mutable.HashMap[K, V]()
      key => cache.getOrElseUpdate(key, fn(key))

    val wouldFallWithoutCached = memoize(wouldFallWithout)

    @tailrec
    final def countFallers(without: Set[Brick], count: Long = 0): Long =
      val wouldFall = wouldFallWithout(without)
      if (wouldFall.isEmpty) count
      else countFallers(wouldFall, count + wouldFall.size)
  }

  def parseInput(input: List[String]): Stack =
    val pattern = """(.+),(.+),(.+)~(.+),(.+),(.+)""".r
    val bricks = input.zipWithIndex.map{
      case (pattern(x1,y1,z1,x2,y2,z2), i) =>
        Brick(Point(x1.toInt,y1.toInt,z1.toInt), Point(x2.toInt,y2.toInt,z2.toInt), (i+65).toChar)
    }
    Stack(bricks.sortBy(_.a.z).toVector)

  def part1(input: List[String]): Long =
    val stack = parseInput(input).fall()
    stack
      .bricks
      .count(b => stack.wouldFallWithout(Set(b)).isEmpty)

  def part2(input: List[String]): Long =
    val stack = parseInput(input).fall()
    stack
      .bricks
      .map(b => stack.countFallers(Set(b)))
      .sum
}
