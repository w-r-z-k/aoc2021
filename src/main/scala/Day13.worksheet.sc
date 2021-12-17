  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day13.example.txt")
  val f = Source.fromFile("../../../data/Day13.txt")

   val input = f.getLines().toArray
   
   val dots = input.takeWhile(_ != "").map{x => x.split(",").toList.map(_.toInt)}.toList.map(x => (x.head, x.tail.head))
   dots
   dots.size

   val folds = input.reverse.takeWhile(_ != "").reverse.map(_.substring(11)).map(_.split("=")).map(x => (x(0), x(1).toInt))
   folds.toList

  def fold(dots: List[(Int, Int)], cmd: (String, Int)): List[(Int, Int)] = {
    val axis = cmd._1(0)
    val loc = cmd._2

    (if (axis == 'y') {
      val dots0 = dots.filter{case (x,y) => y < loc}
      val dots1 = dots.filter{case (x,y) => y > loc}
      val dots2 = for ((x, y) <- dots1) yield (x, (2 * loc) - y)
      (dots0 ++ dots2).distinct
    } else {
      val dots0 = dots.filter{case (x,y) => x < loc}
      val dots1 = dots.filter{case (x,y) => x > loc}
      val dots2 = for ((x, y) <- dots1) yield ((2 * loc) - x, y)
      (dots0 ++ dots2).distinct
    }).distinct
  }

  val dotsFolded = folds.foldLeft(dots){(b, a) => 
    fold(b, a)
  }

  dotsFolded.map(_._1).min
  val w = dotsFolded.map(_._1).max + 1
  dotsFolded.map(_._2).min
  val h = dotsFolded.map(_._2).max + 1
  val matrix = Array.fill(w, h)('.')

  dotsFolded.foreach{ case (x, y) => matrix(x)(y)='#' }
  matrix
  matrix.map(_.mkString).reverse.foreach(println)



  //fold(dots, folds(0))

 
