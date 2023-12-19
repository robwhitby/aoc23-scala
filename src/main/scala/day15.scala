object day15 {

  def hash(s: String): Int = s.foldLeft(0){(a,b) => ((a+b) * 17) % 256}

  def part1(input: String): Int = input.split(',').map(hash).sum

  case class Lens(label: String, fl: Int)

  case class Step(label: String, fl: Option[Int]) {
    def run(boxes: Map[Int,List[Lens]]): Map[Int,List[Lens]] =
      val boxNumber = hash(label)
      val box = boxes.getOrElse(boxNumber, List.empty)
      if (fl.isDefined) {
        val existing = box.indexWhere(_.label == label)
        val lens = Lens(label, fl.get)
        if (existing != -1) boxes.updated(boxNumber, box.updated(existing, lens))
        else boxes.updated(boxNumber, box :+ lens)
      }
      else {
        boxes.updated(boxNumber, box.filterNot(_.label == label))
      }
  }

  object Step {
    def from(s: String): Step = {
      val pattern = """(\w+)[-=](\d*)""".r
      s.match { case pattern(label, fl) => Step(label, fl.toIntOption) }
    }
  }

  def part2(input: String): Int = {

    def power(n: Int, box: List[Lens]): Int =
      box.zipWithIndex.map { (lens, i) => (n + 1) * (i + 1) * lens.fl }.sum

    input
      .split(',')
      .map(Step.from)
      .foldLeft(Map[Int, List[Lens]]())((boxes, step) => step.run(boxes))
      .map(power)
      .sum
  }
}
