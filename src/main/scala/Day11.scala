object Day11 extends App {

  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day11.example.txt")
  val f = Source.fromFile("../../../data/Day11.txt")

  var oct = f.getLines().toArray.map{x => x.map{c => c.toInt - 48}.toArray}

  def flash(oct: Array[Array[Int]], flashed: List[(Int, Int)]): List[(Int, Int)] = {

      var newFlashes: List[(Int, Int)] = List[(Int, Int)]()
      
      // Find all 9's and add them to the list 
      (0 to 9).foreach{c => (0 to 9).foreach{r => {
        if (oct(r)(c) >= 10) {
            oct(r)(c) = 0
            newFlashes = newFlashes :+ (r, c)
         }
      }}}

      // Add one to each nearby octopus
      if (!newFlashes.isEmpty) {
         newFlashes.foreach{case (r,c) => {
            def inc(r: Int, c: Int): Unit = {
               if ((r >= 0 && r <10) && (c >= 0 && c < 10))
                  if (oct(r)(c) != 0) oct(r)(c) = oct(r)(c) + 1
            }
            inc(r-1,c-1); inc(r-1,c  ); inc(r-1,c+1)
            inc(r  ,c-1);               inc(r  ,c+1)
            inc(r+1,c-1); inc(r+1,c  ); inc(r+1,c+1)
         }}
         flash(oct, flashed ++ newFlashes)
      } else
         flashed
  }

  def incAll: Unit = (0 to 9).foreach{c => (0 to 9).foreach{r => oct(r)(c) = oct(r)(c) + 1}}

  println("answer1=" + (1 to 100).foldLeft(0){(b, a) => incAll; b + flash(oct, List()).size })
  
  oct = f.reset().getLines().toArray.map{x => x.map{c => c.toInt - 48}.toArray}
  
  println("answer2=" + (1 to Int.MaxValue).find{step => incAll; 100 == flash(oct, List()).size }.getOrElse(0))
}
