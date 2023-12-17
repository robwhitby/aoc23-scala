import scala.annotation.tailrec

object day4 {

  def part1(input: List[String]): Int = {
    input.map { line =>
      val m = matches(line)
      if (m < 2) m else math.pow(2, m - 1).toInt
    }.sum
  }

  def matches(card: String): Int = {
    val nums = card.split(':')(1).split(Array(' ', '|')).filterNot(_.isBlank).toList
    nums.length - nums.toSet.size
  }

  def part2(input: List[String]): Int = {
    @tailrec
    def rec(lineNumbers: Range, acc: Map[Int, Int] = Map.empty): Map[Int, Int] = {
      if (lineNumbers.isEmpty) acc
      else {
        val n = lineNumbers.head
        val m = matches(input(n))
        val wins = (n + 1 to (n + m).min(input.length - 1)).toList.map{i =>
          (i, acc.getOrElse(i, 0) + (acc.getOrElse(n, 0) + 1))
        }.toMap
        rec(lineNumbers.tail, acc ++ wins)
      }
    }

    val wins = rec(input.indices)
    wins.values.sum + input.length
  }
}