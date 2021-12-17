object Day14 extends App {

  import scala.io.Source

  val f = Source.fromFile("../../../data/Day14.example.txt")
  //val f = Source.fromFile("../../../data/Day14.txt")

  val input = f.getLines()
 
  val template = input.next()
  input.next()

  val rules = input.toList.map(_.split(" -> ").toList).map{x => (x(0), x(1))}.toMap

  def step(polymer: String): String = {
      val pairs = polymer.toList.zip(polymer.toList.tail)

      val inserts = pairs.map{x => rules(x._1.toString + x._2.toString)}

      pairs.zip(inserts).foldLeft("")((b, a) => b + (a._1._1.toString + a._2)) + pairs.last._2.toString
  }

  var polymer = template

  (1 to 20).foreach{i => polymer = step(polymer); println(i); println(polymer)}

  val counts = polymer.toList.groupBy(identity).view.mapValues(_.size).values.toList.sorted

  println("answer1=" + (counts.last - counts.head))

  //println("answer2=" + "")
  //polymer = template

  //(1 to 40).foreach{i => print("."); polymer = step(polymer)}
  //println()
}
