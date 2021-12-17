object Day13 extends App {

  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day13.example.1.txt")
  val f = Source.fromFile("../../../data/Day13.txt")

  val input = f.getLines().toList
 
  val dots = input.takeWhile(_ != "").map{x => x.split(",").toList.map(_.toInt)}.toList.map(x => (x.head, x.tail.head))

  val folds = input.reverse.takeWhile(_ != "").reverse.map(_.substring(11)).map(_.split("=")).map(x => (x(0), x(1).toInt))

  def fold(dots: List[(Int, Int)], cmd: (String, Int)): List[(Int, Int)] = {
    val loc = cmd._2

    val f: ((Int, Int)) => Boolean = if (cmd._1(0) == 'y') {case (x:Int, y:Int) => y < loc} else {case (x,y) => x < loc}

    (dots.filter{f} ++ (
      for ((x, y) <- dots.filter{case(x, y) => !f(x,y) }) yield (if (cmd._1(0) == 'y') (x, (2 * loc) - y) else ((2 * loc) - x, y))
    )).distinct
  }

  println("answer1=" + fold(dots, folds(0)).size)

  val dotsFolded = folds.foldLeft(dots){(b, a) => fold(b, a) }

  val matrix = Array.fill(dotsFolded.map(_._1).max + 1, dotsFolded.map(_._2).max + 1)('.')

  dotsFolded.foreach{ case (x, y) => matrix(x)(y)='#' }

  println("answer2=" + "")

  matrix.map(_.mkString).reverse.foreach(println)
}
