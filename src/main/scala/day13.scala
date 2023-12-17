object day13 {

  def summarize(input: List[String]): Int = {

    def score[T](in: List[T]): Option[Int] = {
      in.indices.drop(1).find { i =>
        val a = in.slice(0, i)
        val b = in.slice(i, i * 2)
        a.reverse.startsWith(b)
      }
    }

    val vertical = score(input).map(_*100)
    val horizontal = score(input.transpose)
    vertical.getOrElse(horizontal.get)
  }


  def parse(input: List[String]): List[List[String]] = {
    input
      .mkString(" ")
      .split("  ")
      .map(_.split(' ').toList)
      .toList
  }

  def part1(input: List[String]): Int = {
    parse(input)
      .map(summarize)
      .sum
  }
}
