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
   polymer
   (1 to 10).foreach{i => polymer = step(polymer)}
   polymer.toList.groupBy(identity).view.mapValues(_.size).toList.sortWith{(a, b) => a._2 < b._2}
   val s = polymer.toList.groupBy(identity).view.mapValues(_.size).toList.sortWith{_._2 > _._2}
    s.head._2 - s.last._2
   val t = polymer.toList.groupBy(identity).view.mapValues(_.size).values.toList.sorted
   t.last - t.head
