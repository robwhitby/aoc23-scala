object day06 {

  case class Race(time: Long, record: Long) {
    def distance(delay: Long) = delay * (time-delay)
  }

  def part1(races: List[Race]): Long = {
    races.map{ r =>
      Range.Long(1, r.time, 1).count(r.distance(_) > r.record)
    }.product
  }

}