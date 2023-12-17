object day03 {
  case class Num(line: Int, start: Int, value: Int)

  def getAllNums(lines: List[String]) = lines.zipWithIndex.flatMap { (line, lineIdx) =>
    def getNums(idx: Int = 0, acc: List[Num] = List.empty): List[Num] = {
      val linePart = line.substring(idx)
      if (linePart.isEmpty) acc
      else {
        val digits = linePart.takeWhile(_.isDigit)
        if (digits.isEmpty) {
          getNums(idx + 1, acc)
        } else {
          getNums(idx + digits.length, acc :+ Num(lineIdx, idx, digits.toInt))
        }
      }
    }
    getNums()
  }

  def part1(input: List[String]): Int = {
    val allNums = getAllNums(input)

    val filtered = allNums.filter{ n =>
      val neighbours = for {
        r <- (n.line-1).max(0) to (n.line+1).min(input.length-1)
        c <- (n.start-1).max(0) to (n.start + n.value.toString.length).min(input(n.line).length-1)
      } yield input(r)(c)
      neighbours.exists(c => !c.isDigit && c != '.')
    }
    filtered.map(_.value).sum
  }

  def part2(input: List[String]) = {
    val allNums = getAllNums(input)

    val gears = input.zipWithIndex.flatMap{ (line, lineIdx) =>
      line.zipWithIndex.filter((c, _) => c == '*').map { (_, idx) =>
        val rows = (lineIdx - 1).max(0) to (lineIdx + 1).min(input.length - 1)
        val cols = (idx - 1).max(0) to (idx + 1).min(input(lineIdx).length - 1)
        allNums.filter { n =>
          val cs = n.start until n.start + n.value.toString.length - 1
          rows.contains(n.line) && cs.start <= cols.end && cols.start <= cs.end
        }
      }
    }

    val filtered = gears.filter(_.length == 2)
    filtered.map(_.map(_.value).product).sum
  }
}