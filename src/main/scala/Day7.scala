object Day7 extends App {

  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day7.example.txt")
  val f = Source.fromFile("../../../data/Day7.txt")

  val numbers = f.getLines().next().split(',').map(_.toInt)

  def medianCalculator(seq: Seq[Int]): Int = {
    //In order if you are not sure that 'seq' is sorted
    val sortedSeq = seq.sortWith(_ < _)
   
    if (seq.size % 2 == 1) sortedSeq(sortedSeq.size / 2)
    else {
      val (up, down) = sortedSeq.splitAt(seq.size / 2)
      (up.last + down.head) / 2
    }
  }
 
  println("answer1=" + numbers.map{x => Math.abs(x - medianCalculator(numbers.toSeq))}.sum)

  def fuel(pos: Int): Int = numbers.map{x => val n = Math.abs(x - pos); (n * (n+1)) / 2}.sum

  val fuels = for (a <- (numbers.min to numbers.max)) yield (a, fuel(a))

  println("answer2=" + fuels.sortBy(x => x._2).head._2)
}
