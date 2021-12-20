import scala.collection.mutable.Map 
  import scala.io.Source

   val f = Source.fromFile("../../../data/Day15.example.txt")
   //val f = Source.fromFile("../../../data/Day15.txt")

  val c = f.getLines().toArray.map { x => x.map(_.toInt - 48).toArray }

  c.foreach { x => x.foreach(print); println() }

  val w = c(0).size // on the x-dimension
  val h = c.size // on the y-dimension
 
  //val g = mutable.Map[(Int, Int), List[(Int, (Int, Int))]]()

  val g = Map[(Int, Int), List[(Int, (Int, Int))]]()

  val gg = for (y <- 0 until h; x <- 0 until w) yield {

    ((x, y), 
       (if (x == w - 1) List() else List((c(y)(x + 1), (x + 1, y))) )  ++
       (if (y == h - 1) List() else List((c(y + 1)(x), (x, y + 1))) )
    )
  }

  g.addAll(gg)
  g.addOne((w - 1, h - 1) -> Nil)

  g((7, 5))

  //val d = Array[Array[Int]]()

  c(0)

  val d = for (y <- 0 until h) yield {
    (for (i <- 0 until 5) yield {c(y).map{n => if (n + (i % h) > 9) n + (i % h) - 9 else n + (i % h)}}).flatten
  }.toArray

  d(0)
  d(1)
  d(2)
  d(4)
  d(5)
  d(6)
  d(7)
  d(8)
  d(9)
  d(0)(49)
  d(1)(49)
  d(2)(49)
  d(4)(49)
  d(5)(49)
  d(6)(49)
  d(7)(49)
  d(8)(49)
  d(9)(49)
  d.size
      //val t = (for (i <- 0 until h) yield {d(0).map{n => if ((n + i) > 9) n + i - 9 else n + i}})
      //t.size
      //t.toArray
      //t
      //t(0).size
  val e = for (j <- 0 until 5 * h) yield {
      d(j % h).map{n => if (n + (j / h) > 9) n + (j / h) - 9 else n + (j / h)}
  }

  e.size
  e(49)(49)
  e(0)
  e(1)
  e(2)
  e(4)
  e(5)
  e(6)
  e(7)
  e(8)
  e(9)
  e(10)(49)
  e(11)(49)

import scala.collection.mutable.ListBuffer

val g1 = for (y <- 0 until h; x <- 0 until w) yield {
   val lb = new ListBuffer[(Int, (Int, Int))]
   if(x != 0)     lb += ((e(y)(x - 1), (x - 1, y)))
   if(x != w - 1) lb += ((e(y)(x + 1), (x + 1, y)))
   if(y != 0)     lb += ((e(y - 1)(x), (x, y - 1)))
   if(y != h - 1) lb += ((e(y + 1)(x), (x, y + 1)))
   lb.toList
}

g1.size

    def QOrdering = new Ordering[(Key, Int)] {
//      def compare(a: (Key, Int), b: (Key, Int)) = b._2.compare(a._2)
      def compare(a: (Key, Int), b: (Key, Int)): Int = {

          if (b._2 < a._2)  -1 
          else if (b._2 > a._2)  1 
          else 0

      } 
    }

  import scala.collection.mutable.TreeSet
    val ts = new TreeSet[(Key, Int)]()(QOrdering)
type Key = (Int, Int)

ts += (((1, 2), 1))
ts += (((1, 3), 1))
ts += (((1, 4), 1))

ts
ts.size


