object Day3 extends App {

  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day3.example.txt")
  val f = Source.fromFile("../../../data/Day3.txt")

  val width = f.getLines().next().length()
  val readings = f.reset().getLines().toList.map(x => Integer.parseInt(x, 2))

  val a:Array[Int] = new Array(width)

  readings.foreach{x => (0 until width).foreach{b => if ((x & (1 << b)) > 0) a(b) = a(b) + 1 }}

  println("answer1=" + 
      (for (i <- 0 until width) yield if (a(i) > readings.length/2) 1<<i else 0).sum
    * (for (i <- 0 until width) yield if (a(i) < readings.length/2) 1<<i else 0).sum)

  var  o2s = readings
  var co2s = readings

  for (i <- width - 1 to 0 by -1) {
      val posmask = 1 << i
      if (o2s.length > 1)
         o2s =  o2s.filter{x => (x & posmask) == (if ( o2s.count{x => (x & posmask) > 0} >= ( o2s.length + 1)/2) 1 else 0) << i}
      if (co2s.length > 1)
         co2s = co2s.filter{x => (x & posmask) == (if (co2s.count{x => (x & posmask) > 0} <= (co2s.length - 1)/2) 1 else 0) << i}
  }
  
  println("answer2=" + (o2s.head * co2s.head))
}
