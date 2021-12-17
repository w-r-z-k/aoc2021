object Day5 extends App {

  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day5.example.txt")
  val f = Source.fromFile("../../../data/Day5.txt")

  val p = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r

  val lines = f.getLines().map{x => val p(x1,y1,x2,y2) = x; ((x1.toInt,y1.toInt),(x2.toInt,y2.toInt))}.toList

  def run(a: ((Int, Int),(Int,Int)) ): List[(Int, Int)] = {
      val xBy = if (a._1._1 < a._2._1) 1 else -1
      val yBy = if (a._1._2 < a._2._2) 1 else -1
      if ((a._1._1 != a._2._1) && (a._1._2 != a._2._2))    // Diagonal
         (a._1._1 to a._2._1 by xBy).zip(a._1._2 to a._2._2 by yBy).toList
      else if (a._1._2 == a._2._2)                         // Horizontal
         (a._1._1 to a._2._1 by xBy).zip(List.fill(Math.abs(a._1._1 - a._2._1)+1)(a._1._2)).toList
      else                                                 // Vertical
         (List.fill(Math.abs(a._1._2 - a._2._2)+1)(a._1._1)).zip(a._1._2 to a._2._2 by yBy)
  }

  println("answer1=" + lines.filter{x => (x._1._1 == x._2._1) || (x._1._2 == x._2._2)}  // remove diagonal lines
                          .flatMap(run).groupBy(identity).view.mapValues(_.size).filter{case (k,v) => v > 1}.size)

  println("answer2=" + lines.flatMap(run).groupBy(identity).view.mapValues(_.size).filter{case (k,v) => v > 1}.size)
}
