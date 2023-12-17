

import scala.annotation.tailrec

object day10 {

  def part1(input: List[String]): Int = {

    val width = input.head.length
    val height = input.length

    def startCell(): Cell = {
      val startY = input.indices.find(input(_).contains('S')).get
      val startX = input(startY).indexOf('S')
      Cell(startX, startY)
    }

    case class Cell(x: Int, y: Int) {
      def content: Char = input(y)(x)
      def inBounds: Boolean = x >= 0 && x < width && y >= 0 && y < height
      def north: Cell = Cell(x, y-1)
      def south: Cell = Cell(x, y+1)
      def east: Cell = Cell(x+1, y)
      def west: Cell = Cell(x-1, y)

      def next(previous: Cell): Cell = {
        this.content match {
          case '|' => Seq(north, south)
          case '-' => Seq(east, west)
          case 'L' => Seq(north, east)
          case 'J' => Seq(north, west)
          case '7' => Seq(south, west)
          case 'F' => Seq(south, east)
          case _ => Seq()
        }
      }.find(_!=previous).get

    }

    val start = startCell()
    val firstCell = Map(
      start.north -> "|F7",
      start.south -> "|LJ",
      start.east -> "-J7",
      start.west -> "-LF",
    ).find((c, pipes) => c.inBounds && pipes.contains(c.content)).map(_._1).get

    @tailrec
    def steps(cell: Cell, previous: Cell = start, count: Int = 0): Int = {
      if (count > 0 && cell == start) count
      else steps(cell.next(previous), cell, count+1)
    }

    math.ceil(steps(firstCell).toFloat / 2).toInt
  }

  def part2(input: List[String]): Int = {

    val width = input.head.length
    val height = input.length

    def startCell(): Cell = {
      val startY = input.indices.find(input(_).contains('S')).get
      val startX = input(startY).indexOf('S')
      Cell(startX, startY)
    }

    case class Cell(x: Int, y: Int) {
      def content: Char = input(y)(x)
      def inBounds: Boolean = x >= 0 && x < width && y >= 0 && y < height
      def north: Cell = Cell(x, y - 1)
      def south: Cell = Cell(x, y + 1)
      def east: Cell = Cell(x + 1, y)
      def west: Cell = Cell(x - 1, y)

      def next(previous: Cell): Cell = {
        this.content match {
          case '|' => Seq(north, south)
          case '-' => Seq(east, west)
          case 'L' => Seq(north, east)
          case 'J' => Seq(north, west)
          case '7' => Seq(south, west)
          case 'F' => Seq(south, east)
          case _ => Seq()
        }
      }.find(_ != previous).get
    }

    val start = startCell()
    val firstCell = Map(
      start.north -> "|F7",
      start.south -> "|LJ",
      start.east -> "-J7",
      start.west -> "-LF",
    ).find((c, pipes) => c.inBounds && pipes.contains(c.content)).map(_._1).get

    @tailrec
    def steps(cell: Cell, previous: Cell = start, cells: List[Cell] = List.empty): List[Cell] = {
      if (cells.nonEmpty && cell == start) cells
      else steps(cell.next(previous), cell, cells :+ cell)
    }

    val edgeList = steps(firstCell)

    //what is content of start cell?
    //it has to connect to firstCell and lastCell
    val firstAndLast = Seq(firstCell, edgeList.last)
    val sN = firstAndLast.find(_ == start.north).map(_ => 'N')
    val sS = firstAndLast.find(_ == start.south).map(_ => 'S')
    val sE = firstAndLast.find(_ == start.east).map(_ => 'E')
    val sW = firstAndLast.find(_ == start.west).map(_ => 'W')
    val startContent = Seq(sN, sS, sE, sW).map(_.getOrElse("")).mkString match {
      case "NS" => '|'
      case "EW" => '-'
      case "NE" => 'L'
      case "NW" => 'J'
      case "SW" => '7'
      case "SE" => 'F'
    }

    val edges = edgeList.toSet + start

    input.indices.map { y =>
      @tailrec
      def rec(x: Int = 0, in: Boolean = false, count: Int = 0): Int = {
        if (x == width) count
        else {
          val cell = Cell(x,y)
          if (edges.contains(cell)) {
            val toggle = "|F7".contains(cell.content) || (cell == start && "|F7".contains(startContent))
            rec(x+1, if (toggle) !in else in, count)
          } else {
            rec(x+1, in, if (in) count+1 else count)
          }
        }
      }
      rec()
    }.sum

  }
}
