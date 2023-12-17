import day11.{Point, Universe}
import day6.Race
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class dayTest extends AnyFunSuite {

  private def open(f: String) = Source.fromURL(getClass.getResource(f)).getLines.toList

  test("1.1") {
    assert(day1.part1(open("/day01ex.txt")) == 142)
    println(day1.part1(open("/day01.txt")))
  }

  test("1.2") {
    assert(day1.part2(open("/day01ex2.txt")) == 281)
    assert(day1.part2(List("eightwo")) == 82)
    println(day1.part2(open("/day01.txt")))
  }

  test("2.1") {
    assert(day2.part1(open("/day02ex.txt"), 12, 13, 14) == 8)
    println(day2.part1(open("/day02.txt"), 12, 13, 14))
  }

  test("2.2") {
    assert(day2.part2(open("/day02ex.txt")) == 2286)
    println(day2.part2(open("/day02.txt")))
  }

  test("3.1") {
    assert(day3.part1(open("/day03ex.txt")) == 4361)
    println(day3.part1(open("/day03.txt")))
  }

  test("3.2") {
    assert(day3.part2(open("/day03ex.txt")) == 467835)
    println(day3.part2(open("/day03.txt")))
  }

  test("4.1") {
    assert(day4.part1(open("/day04ex.txt")) == 13)
    println(day4.part1(open("/day04.txt")))
  }

  test("4.2") {
    assert(day4.part2(open("/day04ex.txt")) == 30)
    println(day4.part2(open("/day04.txt")))
  }

  test("5.1") {
    assert(day5.part1(open("/day05ex.txt")) == 35)
    println(day5.part1(open("/day05.txt")))
  }

  test("5.2") {
    assert(day5.part2(open("/day05ex.txt")) == 46)
    println(day5.part2(open("/day05.txt")))
  }

  test("6.1") {
    val exampleRaces = List(Race(7, 9), Race(15, 40), Race(30, 200))
    assert(day6.part1(exampleRaces) == 288)

    val races = List(Race(59, 597), Race(79, 1234), Race(65, 1032), Race(75, 1328))
    println(day6.part1(races))
  }

  test("6.2") {
    val exampleRaces = List(Race(71530, 940200))
    assert(day6.part1(exampleRaces) == 71503)

    val races = List(Race(59796575, 597123410321328L))
    println(day6.part1(races))
  }

  test("7.1") {
    assert(day7.part1(open("day07ex.txt")) == 6440)
    println(day7.part1(open("day07.txt")))
  }

  test("7.2") {
    assert(day7.part2(open("day07ex.txt")) == 5905)
    println(day7.part2(open("day07.txt")))
  }

  test("8.1") {
    assert(day8.part1(open("day08ex.txt")) == 6)
    println(day8.part1(open("day08.txt")))
  }

  test("8.2") {
    assert(day8.part2(open("day08ex2.txt")) == 6)
    println(day8.part2(open("day08.txt")))
  }

  test("9.1") {
    assert(day9.part1(open("day09ex.txt")) == 114)
    println(day9.part1(open("day09.txt")))
  }

  test("9.2") {
    assert(day9.part2(open("day09ex.txt")) == 2)
    println(day9.part2(open("day09.txt")))
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
}
