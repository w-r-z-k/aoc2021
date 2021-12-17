  import scala.io.Source

  val f = Source.fromFile("../../../data/Day4.example.txt")
  //val f = Source.fromFile("../../../data/Day4.txt")

  val draws = f.getLines().next().split(",").map{x => Integer.parseInt(x)}

  draws.foreach(println)

  val g = f.getLines().grouped(6).toList
  println("answer1=" + g.length)
  val h = g.map{l => l.mkString(" ")}
  h.foreach(println)
  //h.map{x => x.split("\\w").map{x => Integer.parseInt(x)}}
  val j = h.map{x => x.split("\\s+")}
  j.foreach(println)
  println("answer2=" + "")

   val xx = " 2 3   4"

  xx
  xx.split(" ")
  xx.split("\\s+")
  xx.split("\\s+").filter{x => x != ""}
   val z = h.map{x => x.split("\\s+").filter{x => x != ""}.map{x => Integer.parseInt(x)}}
  val zz = z.head
  zz.slice(0, 5)
  zz.groupBy(_ % 5)
  zz.splitAt(5)
  zz.sliding(5, 5).toList.map{x => x.sum}
  zz.sliding(5, 5).toList.map(_.sum)
  zz.grouped(5).toList.map(_.sum)
  zz
  val zzz = (0 until 25).zip(zz).toList
  zzz.groupBy(_._1 % 5).values.map{x => x.map{y => y._2}}.flatten.grouped(5).toList.map(_.sum)
  val w = zzz.groupBy(_._1 % 5).values.map{x => x.map{y => y._2}}
  w
  w.map(_.sum)
  val ww = zzz.groupBy(_._1 % 5)
  zzz.groupBy(_._1 % 5).values.map(_.map(_._2)).map(_.sum)
  val q = (0 until 25).zip(zz).toList.groupBy(_._1 % 5).values.map(_.map(_._2)).map(_.sum).exists(_ == -5)
  val u = (0 until 25).zip(zz).groupBy(_._1 % 5).values.map(_.map(_._2)).map(_.sum)


