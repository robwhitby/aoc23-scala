object day07 {

  case class Hand(cards: String, bid: Int, original: String) extends Ordered[Hand] {
    def typ: Int = {
      cards.groupBy(identity).view.mapValues(_.length).toMap match {
        case c if c.keys.size == 1 => 1 //5 of a kind
        case c if c.values.exists(_ == 4) => 2 //4 of a kind
        case c if c.keys.size == 2 => 3 //full house
        case c if c.values.exists(_ == 3) => 4 //3 of a kind
        case c if c.values.count(_ == 2) == 2 => 5 //2 pair
        case c if c.values.count(_ == 2) == 1 => 6 //1 pair
        case _ => 7
      }
    }

    def compare(that: Hand): Int = {
      val byTyp = that.typ.compareTo(this.typ)
      if (byTyp != 0) byTyp else this.original.compareTo(that.original)
    }

    def best(): Hand = {
      "23456789acde".map { c => this.copy(this.cards.replace('1', c)) }.max
    }
  }

  case object Hand {
    def from(s: String, jokers: Boolean = false): Hand = {
      val fields = s.split(' ')
      val j = if (jokers) '1' else 'b'
      val cards = fields(0).replace('T', 'a').replace('J', j).replace('Q', 'c').replace('K', 'd').replace('A', 'e')
      Hand(cards, fields(1).toInt, cards).best()
    }
  }
  
  def part1(input: List[String]): Int = {
    input.map(Hand.from(_))
      .sorted
      .zipWithIndex
      .map { (h, i) => h.bid * (i + 1) }
      .sum
  }

  def part2(input: List[String]): Int = {
    input.map(Hand.from(_, jokers = true))
      .sorted
      .zipWithIndex
      .map{ (h, i) => h.bid * (i + 1) }
      .sum
  }
}