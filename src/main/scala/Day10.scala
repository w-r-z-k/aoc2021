object Day10 extends App {

  import scala.io.Source
  import scala.collection.mutable.Stack

  //val f = Source.fromFile("../../../data/Day10.example.txt")
  val f = Source.fromFile("../../../data/Day10.txt")

  val input = f.getLines().toList

  val open = Map((')' -> '('), (']' -> '['), ('}' -> '{'), ('>' -> '<'))

  val corrupted = (for (l <- input) yield {
    val s = Stack[Char]()
    l.zipWithIndex.toList.find{case (c, i) => c match {
      case '(' | '[' | '{' | '<' => s.push(c); false
      case ')' | ']' | '}' | '>' => (s.isEmpty || s.pop() != open(c))
    case _ => false
    }}
  })

  val e = corrupted.flatten.map{x => x._1}.groupBy(identity).view.mapValues(_.size).toMap

  val value = Map((')' -> 3), (']' -> 57), ('}' -> 1197), ('>' -> 25137))

  println("answer1=" + e.foldLeft(0){(b, a) => b + (a._2 * (value(a._1) ))})

  val close = open.map{case (k,v) => (v,k)}

  val isCorrupted = corrupted.map{x => x match {case Some(_) => false; case _ => true}}

  val incompletes = input.lazyZip(isCorrupted).collect{case (a,b) if b => a }

  var completions = (for (l <- incompletes) yield {
      val s = Stack[Char]()
      l.foreach{c => c match {
           case '(' | '[' | '{' | '<' => s.push(c)
           case ')' | ']' | '}' | '>' => s.pop()
           case _ =>
      }}
      s
  }).toList.map{x => x.map{c => close(c)}.mkString}

  val points = Map((')' -> 1), (']' -> 2), ('}' -> 3), ('>' -> 4))

  val pc = completions.map{x => x.foldLeft(BigInt(0)){(a, b) => a*5 + points(b)}}

  println("answer2=" + pc.sortWith(_ < _).drop(pc.length/2).head) // median
}
