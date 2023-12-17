import scala.annotation.tailrec

object day9 {

  @tailrec
  def rec(row: Array[Int], acc: Int = 0): Int = {
    val diffs = row.sliding(2).map(a => a(1) - a(0)).toArray
    if (diffs.sum == 0) acc + row.last
    else rec(diffs, acc + row.last)
  }

  def part1(input: List[String]): Int =
    input.map{ line => rec(line.split(' ').map(_.toInt))}.sum

  def part2(input: List[String]): Int =
    input.map{ line => rec(line.split(' ').map(_.toInt).reverse)}.sum

}
