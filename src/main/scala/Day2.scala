object Day2 extends App {

  import scala.io.Source

  //val commands = Source.fromFile("../../../data/Day2.txt").getLines().toList
  val commands = Source.fromFile("../../../data/Day2.example.txt").getLines().toList

  // Keep only the target command; the associated values; then sum them
  def distance (l: List[String], s: String) : Int =
      commands.filter{x => x.startsWith(s)}.map(x => Integer.parseInt(x.stripPrefix(s+" "))).sum

  println("answer1=" + distance(commands, "forward") * (distance(commands, "down") - distance(commands, "up")))

  // Create a list of tuples with the forwards in the 1st elements and the signed down/up in the 2nd element
  //
  //    forward 5  = List((5,  0),
  //    down 5            (0,  5),
  //    forward 8         (8,  0),
  //    up 3              (0, -3),
  //    down 8            (0,  8),
  //    forward 2         (2,  0))  = List((5,0), (0,5), (8,0), (0,-3), (0,8), (2,0))
  
  val p = commands.map{x =>
     if (x.startsWith("forward"))
        (Integer.parseInt(x.stripPrefix("forward ")), 0)
     else 
        (0, Integer.parseInt(
               if (x.startsWith("down"))
                  x.stripPrefix("down ")
               else
                  "-"+x.stripPrefix("up ")
            )
        )
  }
  println("p = "+p)

  val f = p.map(_._1) // Get the forwards = List(5,0,8,0,0,2)

  // The result is then the sum of the forwards (5+0+8+0+0+2=15) times
  //    the forwards zipped with the rolling sum of the down/ups = List((5,0), (0,5), (8,5), (0,2), (0,10), (2,10))
  //    then multliplied together = List(0,0,40,0,0,20) and summed = 60
  //  which is 15 * 60 = 900
 
  println("answer2=" + f.sum * f.zip(p.map(_._2).scanLeft(0)(_ + _).tail).map{case (x, y) => x * y}.sum)

}
