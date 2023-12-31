import scala.annotation.tailrec

object day19 {

  type Workflows = Map[String, Seq[Rule]]

  case class Rule(cat: Char, op: Char, value: Int, dest: String) {
    def run(part: Part): Boolean = (cat, op) match {
      case (' ', _) => true
      case (c, '>') if part(c) > value => true
      case (c, '<') if part(c) < value => true
      case _ => false
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
  
}
