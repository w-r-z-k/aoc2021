  import scala.io.Source

  val f = Source.fromFile("../../../data/Day5.example.txt")
  //val f = Source.fromFile("../../../data/Day5.txt")

  val p = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r

  val lines = f.getLines().map{x => val p(x1,y1,x2,y2) = x; ((x1.toInt,y1.toInt),(x2.toInt,y2.toInt))}.toList
  

  val l = lines.filter{x => (x._1._1 == x._2._1) || (x._1._2 == x._2._2)}
  l
  l.length

  val s = "1,2 -> 3,4"


  val p(x1,y1,x2,y2) = s

  val a = l.head

  def run(a: ((Int, Int),(Int,Int)) ): IndexedSeq[(Int, Int)] =
  if (a._1._1 == a._2._1)
     if (a._1._2 < a._2._2)
        for (y <- a._1._2 to a._2._2) yield (a._1._1, y)
     else
        for (y <- a._2._2 to a._1._2) yield (a._1._1, y)
  else
     if (a._1._1 < a._2._1)
        for (x <- a._1._1 to a._2._1) yield (x, a._1._2)
     else
        for (x <- a._2._1 to a._1._1) yield (x, a._1._2)

  run(l.tail.head).toList
  val z = l.flatMap(run)
  z.groupBy(identity).view.mapValues(_.size).filter{case (k,v) => v > 1}.size

  def run2(a: ((Int, Int),(Int,Int)) ): IndexedSeq[(Int, Int)] = {
      if (((a._1._1 == a._2._1) && (a._1._2 != a._2._2)) ||
          ((a._1._1 != a._2._1) && (a._1._2 == a._2._2)) )
         run(a)
      else {
         val xBy = if (a._1._1 < a._2._1) 1 else -1
         val yBy = if (a._1._2 < a._2._2) 1 else -1
         (a._1._1 to a._2._1 by xBy).zip(a._1._2 to a._2._2 by yBy)
//         run((a._1._1,0),(a._2._1,0)).zip(run((0,a._1._2),(0,a._2._2))).map{case (x, y) => (x._1, y._2)}
      }
  }

  run2(((1,1),(3,3)))
  run2(((3,3),(1,1)))
  run2(((1,0),(3,0)))
  run2(((0,3),(0,1)))
  run2(((5,5),(8,2)))
  lines
  lines.length
  lines.toList.flatMap(run2)
  lines.toList.flatMap(run2).size
  lines.toList.flatMap(run2).groupBy(identity).size
  lines.toList.flatMap(run2).groupBy(identity).view.mapValues(_.size)
  lines.toList.flatMap(run2).groupBy(identity).view.mapValues(_.size).filter{case (k,v) => v > 1}.size

  val f1 = List(1,2,3)
  val f2 = List(0)
  f1.zip(List.fill(f1.length)(0))

  def run3(a: ((Int, Int),(Int,Int)) ): List[(Int, Int)] = {
      val xBy = if (a._1._1 < a._2._1) 1 else -1
      val yBy = if (a._1._2 < a._2._2) 1 else -1
      if (a._1._2 == a._2._2)
         (a._1._1 to a._2._1 by xBy).zip(List.fill(Math.abs(a._1._1 - a._2._1)+1)(a._1._2)).toList
      else
         (List.fill(Math.abs(a._1._2 - a._2._2)+1)(a._1._1)).zip(a._1._2 to a._2._2 by yBy)
  }

  run3((0,5),(6,5))
  run3((0,5),(0,9))
   (0 to 6)
