import scala.collection.immutable.NumericRange
import scala.collection.parallel.CollectionConverters._

object day5 {

  case class Offset(source: NumericRange[Long], offset: Long)

  case object Offset {
    //50 98 2 --> Offset(98 until 98 + 2, 50 - 98),
    def from(line: String): Offset = {
      val nums = line.split(' ').filterNot(_.isBlank).map(_.toLong).toList
      Offset(Range.Long(nums(1), nums(1) + nums(2), 1L), nums(0) - nums(1))
    }
  }

  case class Converter(ranges: List[Offset]) {
    def convert(i: Long): Long = ranges.find(_.source.contains(i)).map(_.offset + i).getOrElse(i)

    def reverse(i: Long): Long = ranges.find(o => o.source.contains(i - o.offset)).map(i - _.offset).getOrElse(i)
  }

  case object Converter {
    def from(lines: List[String]): Converter = Converter(lines.map(Offset.from))
  }

  case class Data(seeds: List[Long], converters: List[Converter] = List.empty)

  def parseInput(input: List[String]): Data = {
    val seeds = input.head.split(' ').toList.tail.map(_.toLong)

    def rec(in: List[String], acc: Map[Int, List[String]] = Map.empty, idx: Int = 0): Map[Int, List[String]] = {
      in match {
        case h :: t if h.isBlank => rec(t, acc, idx)
        case h :: t if h.charAt(0).isLetter => rec(t, acc, idx + 1)
        case h :: t if h.charAt(0).isDigit => rec(t, acc + (idx -> (acc.getOrElse(idx, List.empty) :+ h)), idx)
        case _ => acc
      }
    }

    val converterMap = rec(input)
    val converters = converterMap.keys.toList.sorted.map { key =>
      Converter.from(converterMap(key))
    }

    Data(seeds, converters)
  }

  def part1(input: List[String]): Long = {
    val data = parseInput(input)

    data.seeds.map { seed =>
      data.converters.foldLeft(seed)((x, c) => c.convert(x))
    }.min
  }
  
  def part2(input: List[String]): Long = {
    val data = parseInput(input)
    val seedRanges = data.seeds.grouped(2).toList

    Iterator.iterate(1L)(_+1).find{i =>
      val seed = data.converters.reverse.foldLeft(i)((x, c) => c.reverse(x))
      seedRanges.exists{r => r(0)<=seed && r(0)+r(1)>seed}
    }.getOrElse(0)
  }
}