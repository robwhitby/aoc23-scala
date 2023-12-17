object day13 {

  def parse(input: List[String]): List[List[String]] = {
    input
      .mkString(" ")
      .split("  ")
      .map(_.split(' ').toList)
      .toList
  }

  def summarize(grid: List[String], diff: Int = 0): Option[Int] = {

    def score[T](in: List[T]): Option[Int] = {
      in.indices.drop(1).find { i =>
        val a = in.slice(0, i).reverse.mkString
        val b = in.slice(i, i * 2).mkString
        a.take(b.length).zip(b).count(_ != _) == diff
      }
    }

    val vertical = score(grid).map(_ * 100)
    val horizontal = score(grid.transpose)
    vertical.orElse(horizontal)
  }

  def part1(input: List[String]): Int = {
    parse(input)
      .flatMap(summarize(_))
      .sum
  }

  def part2(input: List[String]): Int = {
    parse(input)
      .flatMap(summarize(_, 1))
      .sum
  }
}
