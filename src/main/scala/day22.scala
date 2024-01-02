import scala.annotation.tailrec

object day22 {

  case class Point(x: Int, y: Int, z: Int)

  case class Brick(a: Point, b: Point, label: Char) {
    val points = (for {x <- a.x to b.x; y <- a.y to b.y; z <- a.z to b.z} yield Point(x, y, z)).toSet

    def downOne() = Brick(a.copy(z = a.z-1), b.copy(z = b.z-1), label)
  }

  case class Stack(bricks: Set[Brick] = Set.empty) {
    def canMove(from: Brick, to: Brick): Boolean = {
      if (to.a.z < 1) return false
      val diff = to.points diff from.points
      !bricks.exists(_.points.exists(diff.contains))
    }

    def fall(): Stack = {
      @tailrec
      def fallBrick(stack: Stack, brick: Brick): Stack = {
        val to = brick.downOne()
        if (stack.canMove(brick, to)) {
          fallBrick(Stack(stack.bricks - brick + to), to)
        }
        else stack
      }

      bricks
        .toSeq
        .sortBy(_.a.z)
        .foldLeft(Stack()){ (s,b) => fallBrick(Stack(s.bricks + b), b) }
    }

    def wouldFall(bs: Set[Brick]): Boolean = bs.exists(b => canMove(b, b.downOne()))
  }

  def parseInput(input: List[String]): Stack = {
    val pattern = """(.+),(.+),(.+)~(.+),(.+),(.+)""".r
    val bricks = input.zipWithIndex.map{
      case (pattern(x1,y1,z1,x2,y2,z2), i) =>
        Brick(Point(x1.toInt,y1.toInt,z1.toInt), Point(x2.toInt,y2.toInt,z2.toInt), (i+65).toChar)
    }
    Stack(bricks.toSet)
  }

  def part1(input: List[String]): Long = {
    val stack = parseInput(input).fall()

    stack.bricks.count{ b =>
      val above = stack.bricks.filter(_.a.z == b.b.z + 1)
      !Stack(stack.bricks - b).wouldFall(above)
    }
  }

  def part2(input: List[String]): Long = {
    ???
  }

}
