object Day17 extends App {

  import scala.io.Source
  import scala.collection.mutable.Queue

  // val f = Source.fromFile("../../../data/Day17.example.txt")
  val f = Source.fromFile("../../../data/Day17.txt")

  val c = f.getLines().next()

  val t = raw"(-*\d+)".r.findAllIn(c).toList.map(_.toInt)
  val tx = t(0) to t(1)
  val ty = t(2) to t(3)

  def hit(x: Int, y: Int): Boolean = tx.contains(x) && ty.contains(y)

  def shoot(vx0: Int, vy0: Int): (Boolean, Int) = {

    var  x: Int = 0  ; var  y: Int = 0
    var vx: Int = vx0; var vy: Int = vy0
    var ymax = Int.MinValue

    while (!hit(x, y) && (x <= tx.max && y >= ty.min)) {
      x = x + vx;  y = y + vy
      vx = vx + (if (vx > 0) - 1 else if (vx < 0) 1 else 0)
      vy = vy - 1
      if (y > ymax) ymax = y
    }

    (hit(x , y), ymax)
  }

  val hits = (for (x <- 0 to tx.max; y <- -100 to 100) yield shoot(x, y))
                 .filter(_._1 == true).map(_._2)

  println("answer1=" + hits.max)
  println("answer2=" + hits.size)
}
