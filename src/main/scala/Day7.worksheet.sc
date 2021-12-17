import spire.syntax.numeric
  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day7.example.txt")
  val f = Source.fromFile("../../../data/Day7.txt")

  val numbers = f.getLines().next().split(',').map(_.toInt)

  
val aSet = Seq(3, 5, 10, 11, 19)
val bSet = Seq(1, 5, 14, 16, 17, 20)
 
 
def medianCalculator(seq: Seq[Int]): Int = {
  //In order if you are not sure that 'seq' is sorted
  val sortedSeq = seq.sortWith(_ < _)
 
  if (seq.size % 2 == 1) sortedSeq(sortedSeq.size / 2)
  else {
    val (up, down) = sortedSeq.splitAt(seq.size / 2)
    (up.last + down.head) / 2
  }
}
 
medianCalculator(aSet) //10
medianCalculator(bSet) //15
medianCalculator(numbers.toSeq) //15

//  val median = numbers.median


numbers.map{x => Math.abs(x - medianCalculator(numbers.toSeq))}.sum

val n = 11
(n * (n+1)) / 2
val opt = 2
def fuel(pos: Int): Int = numbers.map{x => val n = Math.abs(x - pos); (n * (n+1)) / 2}.sum

val max = numbers.max
val min = numbers.min

val fuels = for (a <- (min to max)) yield (a, fuel(a))

fuels.sortBy(x => x._2).head._1
fuels.sortBy(x => x._2).head._2





