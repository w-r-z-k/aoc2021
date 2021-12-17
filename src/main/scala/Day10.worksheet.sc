  import scala.io.Source
  import scala.collection.mutable.Stack

  //val f = Source.fromFile("../../../data/Day10.example.txt")
  val f = Source.fromFile("../../../data/Day10.txt")

  val input = f.getLines().toList

  val stacks = Map('(' -> Stack[Char](),
                   '[' -> Stack[Char](),
                   '{' -> Stack[Char](),
                   '<' -> Stack[Char]())

  val z = input.head
  val l = input.head.length()

  z.toList

  val s = Stack[Char]()

  val open = Map((')' -> '('), (']' -> '['), ('}' -> '{'), ('>' -> '<'))
  val close = open.map{case (k,v) => (v,k)}

  var e = (for (l <- input) yield (
      l.zipWithIndex.toList.find{case (c, i) => c match {
          case '(' | '[' | '{' | '<' => s.push(c); false
          case ')' | ']' | '}' | '>' => (s.isEmpty || s.pop() != open(c))
          case _ => false
        }
      }
  )).flatten.map{x => x._1}.groupBy(identity).view.mapValues(_.size).toMap

  val value = Map((')' -> 3), (']' -> 57), ('}' -> 1197), ('>' -> 25137))

  println("answer1=" + e.foldLeft(0){(b, a) => b + (a._2 * (value(a._1) ))})


  var h = (for (l <- input) yield (
      l.zipWithIndex.toList.find{case (c, i) => c match {
          case '(' | '[' | '{' | '<' => s.push(c); false
          case ')' | ']' | '}' | '>' => (s.isEmpty || s.pop() != open(c))
          case _ => false
        }
      }
  ))
  val corrupted = h.map{x => x match {case Some(_) => false; case _ => true}}

  val ic = input.lazyZip(corrupted).collect{case (a,b) if b => a }
  ic
  ic.size

  var k = (for (l <- ic) yield {
      val s = Stack[Char]()
      l.foreach{c => c match {
           case '(' | '[' | '{' | '<' => s.push(c)
           case ')' | ']' | '}' | '>' => s.pop()
           case _ =>
        }
      }
      s
  }).toList.map{x => x.map{c => close(c)}.mkString}

  val points = Map((')' -> 1), (']' -> 2), ('}' -> 3), ('>' -> 4))

//  val m = k.map{x => x.foldLeft(0){(a, b) => a*5 + points(b)}}
  val m = k.map{x => x.foldLeft(BigInt(0)){(a, b) => a*5 + points(b)}}
  m.sortWith(_ < _).drop(m.length/2).head


    //.flatten.map{x => x._1}.groupBy(identity).view.mapValues(_.size).toMap
