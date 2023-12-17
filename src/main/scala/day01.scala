object day1 {

  def part1(input: List[String]): Int =
    input.map{line => (line.find(_.isDigit).mkString + line.findLast(_.isDigit).mkString).toInt}.sum


  def part2(input: List[String]): Int = {

    val replacements = Map(
      "one" -> "1",
      "two" -> "2",
      "three" -> "3",
      "four" -> "4",
      "five" -> "5",
      "six" -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine" -> "9",
    )

    def replaceNumbers(s: String, acc: String = ""): String =
      if (s.isEmpty) acc else {
        replacements.collectFirst {
          case (k, v) if s.startsWith(k) => replaceNumbers(s.tail, acc + v)
        }.getOrElse(replaceNumbers(s.tail, acc + s.head))
      }

    part1(input.map(replaceNumbers(_)))
  }

}


