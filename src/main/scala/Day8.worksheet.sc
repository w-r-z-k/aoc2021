  import scala.io.Source

  val f = Source.fromFile("../../../data/Day8.example.txt")
  //val f = Source.fromFile("../../../data/Day8.txt")

  val notes = f.getLines().toList

  val r = ".*\\| (.*)".r

  val digits = notes.map{x => x match {case r(y) => s"$y"}}

  val z = digits.map{x => x.split(' ').map{x => x.length}}.flatten.filter{x => x==2 || x==4 || x==3 || x==7}.size
  z

  digits

  val ten = f.reset().getLines().toList

  ten.size
  val r2 = "^(.*) \\| (.*)".r
  val zz = ten.map{x => x match {case r2(x, y) => (x, y)}}

  def deduce(rem: List[Set[Char]], acc: Map[Int, Set[Char]]): Map[Int, Set[Char]] = {

    if (rem.isEmpty)
       return acc
    else {
       val signal = rem.head
       val l = signal.size
       if (l == 2) deduce(rem.tail, acc + (1 -> signal))
       else if (l == 4) deduce(rem.tail, acc + (4 -> signal))
       else if (l == 3) deduce(rem.tail, acc + (7 -> signal))
       else if (l == 7) deduce(rem.tail, acc + (8 -> signal))
       else if (acc.isDefinedAt(1) && ((signal -- acc(1)).size) == 3) deduce(rem.tail, acc + (3 -> signal))
       else if (acc.isDefinedAt(1) && ((signal -- acc(1)).size) == 5) deduce(rem.tail, acc + (6 -> signal))
       else if (acc.isDefinedAt(3) && ((signal -- acc(3)).size) == 2) deduce(rem.tail, acc + (0 -> signal))
       else if (acc.isDefinedAt(6) && ((signal -- acc(6)).size) == 0) deduce(rem.tail, acc + (5 -> signal))
       else if (acc.isDefinedAt(3) && acc.isDefinedAt(6)) {
          if (((signal -- acc(4)).size) == 3)
             deduce(rem.tail, acc + (2 -> signal))
          else
             deduce(rem.tail, acc + (9 -> signal))
       } else deduce(rem.tail :+ rem.head, acc)
    }
  }


  zz.foldLeft(0){(b, a) => b + {
     val x = a._1.split(' ').map(_.toSet).toList
     val y = a._2.split(' ').map(_.toSet).toList
     val v = deduce(x, Map[Int,Set[Char]]())
     y.foldLeft(0){(b, a) => (b*10) + (v.find{case (k, v) => v == a}.get._1)}
  }
  }





