import scala.annotation.tailrec

object day19 {

  type Workflows = Map[String, Seq[Rule]]

  type Ranges = Map[Char, Range]

  def combineRanges(a: Ranges, b: Ranges): Ranges = {
    def get(r: Ranges, key: Char): Range = r.getOrElse(key, (1 to 4000))
    "xmas".map(c =>
      c -> (get(a,c).start max get(b,c).start to (get(a,c).end min get(b,c).end))
    ).toMap
  }


  case class Rule(cat: Char, op: Char, value: Int, dest: String) {
    def run(part: Part): Boolean = (cat, op) match {
      case (' ', _) => true
      case (c, '>') if part(c) > value => true
      case (c, '<') if part(c) < value => true
      case _ => false
    }

    def ranges(): Ranges = op match {
      case '>' => Map(cat -> Range(value + 1, 4000))
      case '<' => Map(cat -> Range(1, value - 1))
      case _ => Map(cat -> Range(0,0))
    }

    def negativeRanges(): Ranges = op match {
      case '<' => Map(cat -> Range(value, 4000))
      case '>' => Map(cat -> Range(1, value))
      case _ => Map(cat -> Range(0,0))
    }
  }

  type Part = Map[Char, Int]

  def parseWorkflows(input: List[String]): Workflows = {
    val ruleRe = """([xmas])(.)(\d+):(.+)""".r
    input.takeWhile(!_.isBlank).map{ line =>
      val name = line.takeWhile(_!='{')
      val rules = line.substring(name.length+1, line.length-1)
        .split(',').toSeq.map{
          case ruleRe(xmas, op, value, dest) => Rule(xmas.head, op.head, value.toInt, dest)
          case s: String => Rule(' ', ' ', 0, s)
        }
      name -> rules
    }.toMap
  }

  def parseParts(input: List[String]): Seq[Part] = {
    val partRe = """\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}""".r
    input.dropWhile(!_.startsWith("{")).map{
      case partRe(x,m,a,s) => Map('x'->x.toInt, 'm'->m.toInt, 'a'->a.toInt, 's'->s.toInt)
    }
  }

  def process(part: Part, workflows: Workflows): Boolean = {
    @tailrec
    def rec(dest: String = "in"): Boolean = dest match {
      case "A" => true
      case "R" => false
      case _ => rec(workflows(dest).find(r => r.run(part)).map(_.dest).getOrElse(""))
    }
    rec()
  }

  def part1(input: List[String]): Int = {
    val workflows = parseWorkflows(input)
    parseParts(input)
      .filter(p => process(p, workflows))
      .map(p => p.values.sum)
      .sum
  }

  def part2(input: List[String]): BigInt = {
    val workflows = parseWorkflows(input)

    def rec(rules: Seq[Rule], ranges: Ranges = Map.empty): Set[Ranges] = {
      val r = rules.head
      if (r.cat == ' ') {
        r.dest match {
          case "A" => Set(ranges)
          case "R" => Set.empty
          case s: String => rec(workflows(s), ranges)
        }
      } else {
        rec(workflows.getOrElse(r.dest, Seq(Rule(' ',' ',0,r.dest))), combineRanges(ranges, r.ranges())) ++
          rec(rules.tail, combineRanges(ranges, r.negativeRanges()))
      }
    }

    val ranges = rec(workflows("in"))
    ranges.map{_.values.map(v => BigInt(v.length)).product}.sum
  }
}
