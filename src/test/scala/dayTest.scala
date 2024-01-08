import day11.{Point, Universe}
import day06.Race
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
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
    val d = System.currentTimeMillis() - t
    if (d < 1000) println(d + "ms") else println(d/1000 + "s")
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

  test("15.1") {
    assert(day15.hash("HASH") == 52)
    assert(day15.part1("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7") == 1320)
    println(day15.part1(open("day15.txt").head))
  }

  test("15.2") {
    assert(day15.part2("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7") == 145)
    println(day15.part2(open("day15.txt").head))
  }

  test("16.1") {
    assert(day16.part1(open("day16ex.txt")) == 46)
    println(day16.part1(open("day16.txt")))
  }

  test("16.2") {
    assert(day16.part2(open("day16ex.txt")) == 51)
    println(day16.part2(open("day16.txt")))
  }

  test("17.1") {
    assert(day17.part1(open("day17ex.txt")) == 102)
    println(day17.part1(open("day17.txt")))
  }

  test("17.2") {
    assert(day17.part2(open("day17ex.txt")) == 94)
    assert(day17.part2(open("day17ex2.txt")) == 71)
    println(day17.part2(open("day17.txt")))
  }

  test("18.1") {
    assert(day18.part1(open("day18ex.txt")) == 62)
    println(day18.part1(open("day18.txt")))
  }

  test("18.2") {
    assert(day18.part2(open("day18ex.txt")) == 952408144115.0)
    println(day18.part2(open("day18.txt")))
  }

  test("19.1") {
    assert(day19.part1(open("day19ex.txt")) == 19114)
    println(day19.part1(open("day19.txt")))
  }

  test("19.2") {
    assert(day19.part2(open("day19ex.txt")) == BigInt(167409079868000L))
    println(day19.part2(open("day19.txt")))
  }

  test("20.1") {
    assert(day20.part1(open("day20ex1.txt")) == 32000000)
    assert(day20.part1(open("day20ex2.txt")) == 11687500)
    println(day20.part1(open("day20.txt")))
  }

  test("20.2") {
    println(day20.part2(open("day20.txt")))
  }

  test("21.1") {
    assert(day21.part1(open("day21ex.txt"), 6) == 16)
    println(day21.part1(open("day21.txt"), 64))
  }

  test("21.2") {
    assert(day21.part2(open("day21ex.txt"), 6) == 16)
    assert(day21.part2(open("day21ex.txt"), 10) == 50)
    assert(day21.part2(open("day21ex.txt"), 5000) == 16733044)
    println(day21.part2(open("day21.txt"), 26501365))
  }

  test("22.1") {
    assert(day22.part1(open("day22ex.txt")) == 5)
    println(time{day22.part1(open("day22.txt"))})
  }

  test("22.2") {
    assert(day22.part2(open("day22ex.txt")) == 7)
    println(time{day22.part2(open("day22.txt"))})
  }

  test("23.1") {
    assert(day23.part1(open("day23ex.txt")) == 94)
    println(day23.part1(open("day23.txt")))
  }

  test("23.2") {
    assert(day23.part2(open("day23ex.txt")) == 154)
    println(day23.part2(open("day23.txt")))
  }

  test("24.1") {
    assert(day24.part1(open("day24ex.txt"), (7,27)) == 2)
    println(day24.part1(open("day24.txt"), (200000000000000L, 400000000000000L)))
  }

  test("24.2") {
    assert(day24.part2(open("day24ex.txt")) == 47)
    println(day24.part2(open("day24.txt")))
  }

  test("25.1") {
    assert(day25.part1(open("day25ex.txt")) == 54)
    println(day25.part1(open("day25.txt")))
  }
}