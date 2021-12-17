  import scala.io.Source

  val f = Source.fromFile("../../../data/Day6.example.txt")
  //val f = Source.fromFile("../../../data/Day6.txt")

  //val ages = f.getLines().next().split(',').map(_.toInt)

  ////val counts = ages.groupBy(identity).view.mapValues(_.size).toMap
  //val counts = ages.groupBy(identity).view.mapValues(_.size).map{case(k,v) => (k -> BigInt(v))}.toMap

  //val counts = f.getLines().next().split(',').groupBy(identity).view.mapValues(_.size).map{case(k,v) => (k -> BigInt(v))}.toMap
  val counts = f.getLines().next().split(',').map(_.toInt).groupBy(x => x).view.mapValues{x => BigInt(x.size)}
  counts
  //f.getLines().next().split(',').map{x => BigInt(x)}.groupBy(x => x).view
