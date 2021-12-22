import scala.collection.mutable.Map 
  import scala.io.Source

   val f = Source.fromFile("../../../data/Day17.example.txt")
   //val f = Source.fromFile("../../../data/Day17.txt")

  val c = f.getLines().next()

//  val r = raw".*(\d+).*(\d+).*(\d+).*(\d+)".r
val r = raw"(-*\d+)".r
val all = r.findAllIn(c).toList
val t = raw"(-*\d+)".r.findAllIn(c).toList.map(_.toInt)
val tx = t(0) to t(1)
val ty = t(2) to t(3)

  List(true, true, false, true, false).filter(_ == true)

  val x = (1 to 100)
  x.contains(0)
  x.contains(100)
  x.contains(101)
  x.contains(1)
//  val t = (-10 to -5)
