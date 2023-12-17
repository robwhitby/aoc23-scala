object day2 {
  def part1(input: List[String], rmax: Int, gmax: Int, bmax: Int): Int = {
    val games = input.map(Game.from).filter{ game =>
      game.r <= rmax && game.g <= gmax && game.b <= bmax
    }
    games.map(_.id).sum
  }

  def part2(input: List[String]): Int = {
    val games = input.map(Game.from)
    games.map(game => game.r.max(1) * game.g.max(1) * game.b.max(1)).sum
  }
  
  case class Game(id: Int, r: Int, g: Int, b: Int)
  case object Game {
    def from(s: String): Game = {
      val id = s.split(':')(0).filter(_.isDigit).toInt
      def colour(col: String) = ("""(\d+) """ + col).r.findAllMatchIn(s).map(_.group(1).toInt).max
      Game(id, colour("red"), colour("green"), colour("blue"))
    }
  }

}
