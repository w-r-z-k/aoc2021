object Day12 extends App {

  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day12.example.1.txt")
  val f = Source.fromFile("../../../data/Day12.txt")

  val g = (for(l <- f.getLines().toList) yield { // this reads & creates the cave map
    val a = l.split("-")
    List((a(0), a(1)), (a(1), a(0)))
  }).flatten.groupBy(_._1).map(v => (v._1, v._2.map(_._2)))
  
  def traverse(s: String, paths: List[List[String]], ok2: Boolean): List[List[String]] = {

     def visit(s: String, cp: List[String], ok2: Boolean): Boolean = {
       // ok2 == true if you are allowed to visit one small cave twice (i.e. Part 2)
       !cp.contains(s) ||  // if the current path doesn't yet contain the next cave
       // go through the current path; remove the large caves; group and count the small caves
       (ok2 && !cp.filter{x => !("[A-Z].*".r.matches(x))}.groupBy(identity).view.mapValues(_.size)
              .toMap.values.toList.exists{x:Int => x > 1})  // and test if any are already in 2x
     } // return true if it's OK to visit (i.e. ok to add the current cave to the current path

    val cp = paths.head // we keep the path current being built at the head of the path list
  
    if (s != "end") {
  
       val cp1 = cp :+ s // the new current path is the current path plus the next cave
  
       (for (n <- g(s) if (n != "start") && ((visit(n, cp1, ok2) || ("[A-Z].*".r.matches(n))))) yield {
          traverse(n, (cp1 +: paths.tail), ok2)
       }).flatten
    } else
       (cp :+ s) +: paths.tail // the current path and the latest cave plus the other paths
  }

  println("answer1=" + traverse("start", List(List()), false).size)
  println("answer2=" + traverse("start", List(List()), true).size)
}
