import scala.collection.mutable

object day12 {

  def parse(input: List[String]): Seq[(List[Char], Seq[Int])] = {
    input.map{ line =>
      val Array(a,b) = line.split(' ')
      (a.toList, b.split(',').map(_.toInt).toSeq)
    }
  }

  private val cache = mutable.HashMap[(List[Char], Seq[Int]), Long]()

  def memoize(key: (List[Char], Seq[Int]))(block: => Long) =
    cache.getOrElseUpdate(key, block)

  def rec(springs: List[Char], groups: Seq[Int]): Long = memoize(springs,groups) {
    if (springs.isEmpty && groups.isEmpty) 1
    else springs match {
      case '.' :: tail => rec(tail, groups)
      case '?' :: tail => rec(tail, groups) + rec('#' :: tail, groups)
      case '#' :: _ if groups.isEmpty => 0
      case '#' :: _ if springs.takeWhile(_ != '.').length < groups.head => 0
      case '#' :: _ => springs.drop(groups.head) match {
        case '#' :: _ => 0
        case _ :: tail => rec(tail, groups.tail)
        case Nil => rec(List.empty, groups.tail)
      }
      case _ => 0
    }
  }

  def part1(input: List[String]): Long =
    parse(input)
      .map(rec)
      .sum

  def expand(row: (List[Char], Seq[Int])): (List[Char], Seq[Int]) =
    ((s"?${row._1.mkString}" * 5).tail.toList, Seq.fill(5)(row._2).flatten)

  def part2(input: List[String]): Long = {
    parse(input)
      .map(expand)
      .map(rec)
      .sum
  }

}
