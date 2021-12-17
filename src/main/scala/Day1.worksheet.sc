
import scala.io.Source

// implicit class IntToBase( val digits:String ) extends AnyVal {
//   def base(b:Int) = Integer.parseInt( digits, b )
//   def b = base(2)
//   def o = base(8)
//   def x = base(16)
// }

// val lines = Source.fromFile("Day5.data.example").getLines.map(s => s.map(c => if(c == 'F' || c == 'L') '0' else '1').b).max
val seatIDs = Source.fromFile("Day5.data").getLines().
      map(s => Integer.parseInt(
           s.map(c => if(c == 'F' || c == 'L') '0' else '1')
                                ,2
                                )
          ).toList

println("answer1="+seatIDs.max)
println("answer1="+seatIDs.min)
println("answer2="+((0 to 127).sum - (0 until seatIDs.min).sum - seatIDs.sum))

val m1 = List(1, 2, 3) 
val result = m1.min
val result1 = m1.max

(0 to (127*8)).sum
(0 to 15).sum
120 - 6 - 29
(4 to 15).sum
