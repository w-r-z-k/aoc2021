object Day4 extends App {

  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day4.example.txt")
  val f = Source.fromFile("../../../data/Day4.txt")

  val draws = f.getLines().next().split(",").map{x => Integer.parseInt(x)}

  val g = f.getLines().grouped(6).toList
  val boards0 = g.map{l => l.mkString(" ")}.map{x => x.split("\\s+").filter{x => x != ""}.map{x => Integer.parseInt(x)}}
  var boards = boards0

  def isBingo(board: Array[Int]) : Boolean = {
    if (board.grouped(5).toList.map(_.sum).exists( _ == -5 )) { // BINGO! horizontal
       true
     } else if ((0 until 25).zip(board).groupBy(_._1 % 5).values.map(_.map(_._2)).map(_.sum).exists(_ == -5)) { // BINGO! vertical
       true
     } else
       false
  }

  var winner = -1
  val bingo = draws.find{draw =>
    // Mark all boards
    boards.foreach{board =>
      val i = board.indexOf(draw)
      if (i != -1) board(i) = -1
    }
    // Check all boards
    winner = boards.indexWhere{board => isBingo(board)}
    winner != -1
  }
  println("answer1=" + bingo.get * boards(winner).map{x => if (x == -1) 0 else x}.sum )

  import scala.collection.mutable.ListBuffer

  boards = g.map{l => l.mkString(" ")}.map{x => x.split("\\s+").filter{x => x != ""}.map{x => Integer.parseInt(x)}}

  val v = new ListBuffer[Array[Int]] () // build a list of boards that bingo

  val bingoLast = draws.find{draw =>
    // Mark all boards
    boards.foreach{board =>
      val i = board.indexOf(draw)
      if (i != -1) board(i) = -1
    }
    // Check all un-bingo-ed boards
    boards.foreach{board =>
      if (!v.exists{_ == board})
         if (isBingo(board)) v += board
    }
    v.length == boards.length
  }

  println("answer2=" + bingoLast.get * v.reverse.head.map{x => if (x == -1) 0 else x}.sum )
}
