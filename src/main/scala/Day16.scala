object Day16 extends App {

  import scala.io.Source
  import scala.collection.mutable.Queue

  // val f = Source.fromFile("../../../data/Day16.example.txt")
  val f = Source.fromFile("../../../data/Day16.txt")

  val c = f.getLines().next()
           .map{x => Integer.parseInt(x.toString, 16).toBinaryString
                .reverse.padTo(4, "0").reverse}.flatten.mkString.toList

  var q = Queue[Char](); q ++= c.mkString // the bitstream

  def bits(len: Int): String = (for (_ <- 0 until len) yield q.dequeue()).mkString

  def number(len: Int): Int = Integer.parseInt(bits(len), 2)

  def calculate(ptype: Int, values: List[Long]): Long = { // fn table?
    ptype match { case 0 => values.sum; case 1 => values.product
                  case 2 => values.min; case 3 => values.max
                  case 5 => if (values.head > values.tail.head) 1 else 0
                  case 6 => if (values.head < values.tail.head) 1 else 0
                  case 7 => if (values.head == values.tail.head) 1 else 0}}

  def packet(n: Int, op: Int, vacc: Int, acc: Long): (Int, Long) = {
    if (n > 1) {// get packets sequentially then apply the parents operator
      val l = for (i <- 1 to n) yield packet(1, 0, 0, 0)
      (l.map(_._1).sum + vacc, calculate(op, l.map(_._2).toList))
    } else {// we're getting a single hierarchical packet
      val version = number(3)
      val ptype = number(3)
      if (ptype == 4) { // this is a literal value
        var lead = 0
        val sb = new StringBuilder()
        do {
          lead = number(1)
          sb ++= bits(4)
        } while (lead != 0)
        val value = java.lang.Long.parseLong(sb.result(), 2)
        (vacc + version, value) // no recurse
      } else { // it's an operator packet
        if (number(1) == 0) { // ... by number of bits in sub-packets
          var length = number(15)
          val q0 = q         // store a copy of the bitstream
          q = q.take(length) // take the length of the bitstream needed

          var l = List[(Int, Long)]() 
          do { // get pack-by-packet until the bitstream is done
            l = l :+ packet(1, 0, 0, 0)
          } while (!q.isEmpty)

          q = q0       // restore the bitstream
          bits(length) // but waste the used up bits
          (vacc + l.map(_._1).sum + version, calculate(ptype, l.map(_._2)))
        } else { // ... by number of sub-packets
          var npackets = number(11)
          packet(npackets, ptype, vacc + version, acc)// get # of packets needed
        }
      }
    }
  }

  val (ans1, ans2) = packet(1, 0, 0, 0)// 1 packet, 0:==summed, version0, value0
  println("answer1=" + ans1)
  println("answer2=" + ans2)
}
