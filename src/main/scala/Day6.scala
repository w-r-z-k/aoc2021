object Day6 extends App {

  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day6.example.txt")
  val f = Source.fromFile("../../../data/Day6.txt")

  // counts is a map of #Days Remaining Before Spawning => #fish in that state (as a BigInt)
  val counts = f.getLines().next().split(',').map(_.toInt).groupBy(x => x).view.mapValues{x => BigInt(x.size)}.toMap

  def countFish(start: Map[Int, BigInt], days: Int): BigInt = {
    var d = 1

    var c:Map[Int, BigInt] = start

    while (d <= days) {
      val c1 = for ((k, v) <- c) yield (k-1 -> v) // decrement all states' days remaining (the key)
      c = if (c1.isDefinedAt(-1)) { // if any fish need to spawn the new map is ...
        // new fish with 8 days ++ fish with 6 days plus fish that just spawned with 6 days ++ the rest of the map
        Map(8 -> c1(-1)) ++ Map(6 -> (c1(-1)+c1.getOrElse(6,0))) ++ c1.removed(-1).removed(6)
      } else // otherwise the just decremented map is the map for the next day
        c1
      d = d + 1
    }

    c.foldLeft(BigInt(0))(_+_._2) // return the sum of the number of fish in all of the states
  }

  println("answer1=" + countFish(counts, 80))
  println("answer2=" + countFish(counts, 256))
}
