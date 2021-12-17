  import scala.io.Source

  val f = Source.fromFile("../../../data/Day11.example.txt")
  //val f = Source.fromFile("../../../data/Day11.txt")

  val input = f.getLines().toArray

  input.foreach(println)

  val oct = input.map{x => x.map{c => c.toInt - 48}.toArray}

  val oct0 = oct.clone()


  (0 to 9).foreach{c => (0 to 9).foreach{r => oct(r)(c) = oct(r)(c) + 1}}


  oct.head
  oct.tail.head

  def flash(oct: Array[Array[Int]], flashed: List[(Int, Int)]): List[(Int, Int)] = {
      var newFlashes: List[(Int, Int)] = List[(Int, Int)]()
      

      // Add one to each nearby octopus
      if (!newFlashes.isEmpty) {
        newFlashes.foreach{case (r,c) => {
            def inc(r: Int, c: Int): Unit = {
               if (oct(r)(c) != 0) oct(r)(c) = oct(r)(c) + 1
            }
            inc(r-1,c-1)
            inc(r-1,c  )
            inc(r-1,c+1)
            inc(r  ,c-1)
            inc(r  ,c+1)
            inc(r+1,c-1)
            inc(r+1,c  )
            inc(r+1,c+1)
         }}
      }

      flashed ++ newFlashes

  }

      // Find all 9's and add them to the list 
      (0 to 9).foreach{c => (0 to 9).foreach{r => {
         if (oct(r)(c) == 9) {
            oct(r)(c) = 0
            newFlashes = newFlashes :+ (r, c)
         }
      }}}
      
  flash(oct, List())
