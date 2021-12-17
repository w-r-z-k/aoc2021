object Day1 extends App {

  import scala.io.Source

  val depths = Source.fromFile("../../../data/Day1.txt").getLines().map(s => Integer.parseInt(s)).toList
  // val depths = Source.fromFile("../../../data/Day1.example.txt").getLines().map(s => Integer.parseInt(s)).toList

  val answer1 = (depths.tail).zip(depths).map{case (x, y) => x - y}.count{p => p > 0}

  println("answer1=" + answer1)

  val depths2 = (depths.tail.tail).zip((depths.tail).zip(depths)).map{case (x, (y, z)) => x + y + z}

  val answer2 = (depths2.tail).zip(depths2).map{case (x, y) => x - y}.count{p => p > 0}

  println("answer2=" + answer2)
}
