import day11.{Point, Universe}
import day06.Race
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class dayTest extends AnyFunSuite {

  private def open(f: String) = Source.fromURL(getClass.getResource(f)).getLines.toList

  test("1.1") {
    assert(day01.part1(open("day01ex.txt")) == 142)
    println(day01.part1(open("day01.txt")))
  }

  test("1.2") {
    assert(day01.part2(open("day01ex2.txt")) == 281)
    assert(day01.part2(List("eightwo")) == 82)
    println(day01.part2(open("day01.txt")))
  }

  test("2.1") {
    assert(day02.part1(open("day02ex.txt"), 12, 13, 14) == 8)
    println(day02.part1(open("day02.txt"), 12, 13, 14))
  }

  test("2.2") {
    assert(day02.part2(open("day02ex.txt")) == 2286)
    println(day02.part2(open("day02.txt")))
  }

  test("3.1") {
    assert(day03.part1(open("day03ex.txt")) == 4361)
    println(day03.part1(open("day03.txt")))
  }

  test("3.2") {
    assert(day03.part2(open("day03ex.txt")) == 467835)
    println(day03.part2(open("day03.txt")))
  }

  test("4.1") {
    assert(day04.part1(open("day04ex.txt")) == 13)
    println(day04.part1(open("day04.txt")))
  }

  test("4.2") {
    assert(day04.part2(open("day04ex.txt")) == 30)
    println(day04.part2(open("day04.txt")))
  }

  test("5.1") {
    assert(day05.part1(open("day05ex.txt")) == 35)
    println(day05.part1(open("day05.txt")))
  }

  test("5.2") {
    assert(day05.part2(open("day05ex.txt")) == 46)
    println(day05.part2(open("day05.txt")))
  }

  test("6.1") {
    val exampleRaces = List(Race(7, 9), Race(15, 40), Race(30, 200))
    assert(day06.part1(exampleRaces) == 288)

    val races = List(Race(59, 597), Race(79, 1234), Race(65, 1032), Race(75, 1328))
    println(day06.part1(races))
  }

  test("6.2") {
    val exampleRaces = List(Race(71530, 940200))
    assert(day06.part1(exampleRaces) == 71503)

    val races = List(Race(59796575, 597123410321328L))
    println(day06.part1(races))
  }

  test("7.1") {
    assert(day07.part1(open("day07ex.txt")) == 6440)
    println(day07.part1(open("day07.txt")))
  }

  test("7.2") {
    assert(day07.part2(open("day07ex.txt")) == 5905)
    println(day07.part2(open("day07.txt")))
  }

  test("8.1") {
    assert(day08.part1(open("day08ex.txt")) == 6)
    println(day08.part1(open("day08.txt")))
  }

  test("8.2") {
    assert(day08.part2(open("day08ex2.txt")) == 6)
    println(day08.part2(open("day08.txt")))
  }

  test("9.1") {
    assert(day09.part1(open("day09ex.txt")) == 114)
    println(day09.part1(open("day09.txt")))
  }

  test("9.2") {
    assert(day09.part2(open("day09ex.txt")) == 2)
    println(day09.part2(open("day09.txt")))
  }

  test("10.1") {
    assert(day10.part1(open("day10ex.txt")) == 8)
    println(day10.part1(open("day10.txt")))
  }

  test("10.2") {
    assert(day10.part2(open("day10ex2.txt")) == 8)
    println(day10.part2(open("day10.txt")))
  }

  test("11") {
    val u = Universe(List.empty, 1)
    assert(u.distance(Point(0,0), Point(2,2)) == 4)

    val u10 = Universe(List("#.#","...","..#"), 10)
    assert(u10.distance(Point(0, 0), Point(2, 2)) == 22)

    val ux = Universe(open("day11ex.txt"), 2)
    assert(ux.distance(Point(0,2), Point(9,6)) == 17)
  }

  test("11.1") {
    assert(day11.part1(open("day11ex.txt")) == 374)
    println(day11.part1(open("day11.txt")))
  }

  test("11.2") {
    assert(day11.part2(open("day11ex.txt"), 100) == 8410)
    println(day11.part2(open("day11.txt"), 1000000))
  }

  def time[T](block: => T): T = {
    val t = System.currentTimeMillis()
    val b = block
    println(System.currentTimeMillis() - t + "ms")
    b
  }

  test("12.1") {
    assert(day12.part1(open("day12ex.txt")) == 21)
    println(time{ day12.part1(open("day12.txt")) })
  }

  test("12.2") {
    assert(day12.expand(
      ("#.".toList, Seq(1,2,3))) ==
      ("#.?#.?#.?#.?#.".toList, Seq(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)))

    assert(time(day12.part2(open("day12ex.txt"))) == 525152)
    println(day12.part2(open("day12.txt")))
  }

  test("13.1") {
    assert(day13.part1(open("day13ex.txt")) == 405)
    println(day13.part1(open("day13.txt")))
  }

  test("13.2") {
    assert(day13.part2(open("day13ex.txt")) == 400)
    println(day13.part2(open("day13.txt")))
  }

  test("14") {
    import day14.Grid
    val ex = open("day14ex.txt")
    assert(List("..O..#O..O").tilt == List("....O#..OO"))
    assert(List("#.#..O#.##").tilt == List("#.#..O#.##"))
    assert(ex.rotate90.rotate90.rotate90.rotate90 == ex)
    assert(ex.tiltNorth == open("day14ex-north.txt"))
    assert(ex.cycle == open("day14ex-cycle.txt"))
  }

  test("14.1") {
    assert(day14.part1(open("day14ex.txt")) == 136)
    println(day14.part1(open("day14.txt")))
  }

  test("14.2") {
    assert(day14.part2(open("day14ex.txt")) == 64)
    println(day14.part2(open("day14.txt")))
  }
}
