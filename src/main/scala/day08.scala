import scala.annotation.tailrec

object day08 {
  case class Data(commands: String, network: Map[String, (String, String)]) {
    def next(from: String, dir: Char): String =
      if (dir == 'L') this.network(from)._1 else this.network(from)._2
  }

  def parse(input: List[String]): Data = {
    Data(
      commands = input.head,
      network = input.drop(2).map{ line =>
        val List(k, l, r) = line.replaceAll("[^A-Z0-9]", "").grouped(3).toList
        k -> (l, r)
      }.toMap
    )
  }

  def part1(input: List[String]): Int = {
    val data = parse(input)

    @tailrec
    def rec(commands: String, from: String, count: Int = 0): Int = {
      if (from == "ZZZ") count
      else rec(if (commands.length > 1) commands.tail else data.commands, data.next(from, commands.head), count+1)
    }

    rec(data.commands, "AAA")
  }

  def part2(input: List[String]): Long = {
    val data = parse(input)

    @tailrec
    def rec(commands: String, from: String, count: Int = 0): Int = {
      if (from.endsWith("Z")) count
      else rec(if (commands.length > 1) commands.tail else data.commands, data.next(from, commands.head), count+1)
    }

    def gcd(i: Long, j: Long): Long = j match {
      case 0 => i
      case _ => gcd(j, i % j)
    }

    def lcm(i: Long, j: Long) = i * j / gcd(i, j)

    data.network.keys.filter(_.endsWith("A")).map{ n =>
      rec(data.commands, n).toLong
    }.foldLeft(1L)(lcm)
  }
}
