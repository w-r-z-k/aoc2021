object Day9 extends App {

  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day9.example.txt")
  val f = Source.fromFile("../../../data/Day9.txt")

  val floor = f.getLines().toList.map(x => x.toArray.map(_.toInt - 48))

  val w = floor.head.length
  val h = floor.length

  def isLowPoint(r: Int, c: Int): Boolean = {
    val p = floor(r)(c)
    (   ((c == 0) || floor(r)(c-1) > p) && ((c == w-1) || floor(r)(c+1) > p)
     && ((r == 0) || floor(r-1)(c) > p) && ((r == h-1) || floor(r+1)(c) > p)
    )
  }

  val lowPoints = for (r <- (0 until h);
                       c <- floor(r).zipWithIndex if isLowPoint(r, c._2))
                  yield (c._1, (r, c._2))

  println("answer1=" + lowPoints.foldLeft(0){(b, a) => b + (1 + a._1)})

  def basins(start: (Int, Int), basin: List[(Int, Int)]): List[(Int, Int)] = {

    var bs = basin :+ start
    
    var r = start._1 - 1; var c = start._2

    if ((start._1 > 0    ) && (floor(r)(c)) != 9 && !bs.contains((r,c))) bs = basins((r,c), bs)
    r = start._1 + 1                                                               
    if ((start._1 < h - 1) && (floor(r)(c)) != 9 && !bs.contains((r,c))) bs = basins((r,c), bs)
    r = start._1; c = start._2 - 1                                                 
    if ((start._2 > 0    ) && (floor(r)(c)) != 9 && !bs.contains((r,c))) bs = basins((r,c), bs)
    r = start._1; c = start._2 + 1                                                 
    if ((start._2 < w - 1) && (floor(r)(c)) != 9 && !bs.contains((r,c))) bs = basins((r,c), bs)

    bs
  }

  val bs = for (lp <- lowPoints) yield basins(lp._2, List())

  println("answer2=" + bs.map{x => x.size}.sortWith(_ > _).take(3).product)
}
