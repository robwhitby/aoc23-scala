import scala.collection.mutable.ArrayBuffer

object day25:

  type Matrix = Vector[Vector[Int]]

  def parseInput(input: List[String]): Map[Int, Set[Int]] =
    val pairs = input.toSet.flatMap{ line =>
      val fields = line.replace(":","").split(" ").toSeq
      fields.tail.map(Set(fields.head, _)).toSet
    }
    val index = pairs.flatten.toSeq.sorted.zipWithIndex.toMap
    index.map{ (k,v) =>
      v -> pairs.filter(_.contains(k)).flatMap(_ - k).map(index)
    }

  def toMatrix(graph: Map[Int, Set[Int]]): Matrix =
    (0 until graph.size).map{ y =>
      (0 until graph.size).map { x => if graph(x).contains(y) then 1 else 0 }.toVector
    }.toVector

  // https://en.wikipedia.org/wiki/Stoer-Wagner_algorithm#Example_code
  def minCut(matrix: Matrix): (Int, Seq[Int]) = {
    val mat = matrix.map(_.toArray).toArray
    var best = (Int.MaxValue, ArrayBuffer[Int]())
    val n = mat.length
    val co = ArrayBuffer[ArrayBuffer[Int]]()
    for (i <- 0 until n) co.addOne(ArrayBuffer(i))
    for (ph <- 1 until n) {
      var (w, s, t) = (mat(0).clone(), 0, 0)
      for (it <- 0 until n - ph) {
        w(t) = Int.MinValue
        s = t
        t = w.indexOf(w.max)
        for (i <- 0 until n) w(i) += mat(t)(i)
      }
      val newBest = (w(t) - mat(t)(t), co(t))
      if (newBest._1 < best._1) best=newBest
      co(s).addAll(co(t))
      for (i <- 0 until n) {
        mat(s)(i) += mat(t)(i)
        mat(i)(s) = mat(s)(i)
      }
      mat(0)(t) = Int.MinValue
    }
    (best._1, best._2.toSeq)
  }

  def part1(input: List[String]): Int =
    val matrix = toMatrix(parseInput(input))
    val cut = minCut(matrix)
    cut._2.length * (matrix.length - cut._2.length)