import scala.collection.mutable.Map 
  import scala.io.Source

   val f = Source.fromFile("../../../data/Day16.example.txt")
   //val f = Source.fromFile("../../../data/Day16.txt")

  val c = f.getLines().next().map {x => Integer.parseInt(x.toString, 16).toBinaryString.reverse.padTo(4,"0").reverse}
       .flatten.mkString.toList

       import scala.collection.mutable.Queue
       var q = Queue[Char]()

       q ++= c

       // (val cc, q) = q.dequeue
       var q0 = q
       q = q.take(3)
       q
       q0

// val version = Integer.parseInt((for (_ <- 0 until 3) yield q.dequeue()).mkString, 2)
// val ptype = Integer.parseInt((for (_ <- 0 until 3) yield q.dequeue()).mkString, 2)
//1 to 2
 // if (ptype == 6) {
 //
 // } else {
 //   val ltype = Integer.parseInt(q.dequeue().toString, 2)
 // }

val l = List(9)
l.product

// Integer.parseInt("0111001100011011111", 2)
java.lang.Long.parseLong("011100110110100000110001000011011111", 2)
