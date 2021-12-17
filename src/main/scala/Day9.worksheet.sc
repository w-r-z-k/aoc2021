  import scala.io.Source

  val f = Source.fromFile("../../../data/Day9.example.txt")
  //val f = Source.fromFile("../../../data/Day9.txt")

  val input = f.getLines().toArray

  val floor = input.map(x => x.toArray.map(_.toInt - 48))

  for (r <- floor; c <- r) yield (c)

  floor.length
  for (r <- (0 until floor.length); c <- floor(r)) yield (c)
  floor.head.length

  val elements = List("a", "b", "c", "d")

  for (e <- elements; i <- 0 until elements.length) yield (e, i)

  val w = floor.head.length
  val h = floor.length

  def isLowPoint(r: Int, c: Int): Boolean = {
    val p = floor(r)(c)
    (   ((c == 0) || floor(r)(c-1) > p)
     && ((c == w-1) || floor(r)(c+1) > p)
     && ((r == 0) || floor(r-1)(c) > p)
     && ((r == h-1) || floor(r+1)(c) > p)
    )
  }

  val lowPoints = for (r <- (0 until h);
       //c <- floor(r).zipWithIndex) yield (c._1, (r, c._2))
       c <- floor(r).zipWithIndex if isLowPoint(r, c._2)) yield (c._1, (r, c._2))

  lowPoints.foldLeft(0){(b, a) => b + (1 + a._1)}

  def countBasin(start: (Int, Int), basin: List[(Int, Int)]): List[(Int, Int)] = {
    // From the starting point check all around
    // Recurse only if the point is not edge, not 9, and not already in the basin
    // otherwise return the basin
    if ((start._1 >= 0) && (start._1 <  h) && (start._2 >= 0) && (start._2 <  w)) {
       if ((floor(start._1)(start._2) != 9) && (!basin.contains(start))) {
          if (start._1 >= 0) countBasin((start._1 - 1, start._2    ), basin :+ start)
          if (start._1 <  h) countBasin((start._1 + 1, start._2    ), basin :+ start)
          if (start._2 >= 0) countBasin((start._1    , start._2 - 1), basin :+ start)
          if (start._2 <  w) countBasin((start._1    , start._2 + 1), basin :+ start)
       }
    }
    basin
  }

  countBasin(lowPoints.head._2, List())

  




