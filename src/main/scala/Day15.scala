object Day15 extends App {

  import scala.collection.mutable.Map
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.PriorityQueue
  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day15.example.txt")
  val f = Source.fromFile("../../../data/Day15.txt")

  case class Key (x: Int, y: Int)

  def Dijkstra( Graph: Key => List[(Int, Key)], source: Key, target: Key, w: Int, h: Int): Int = {

    val dist = HashMap[Key, Int]()

    val Q = new PriorityQueue[(Key, Int)]()(
       new Ordering[(Key, Int)] {                                     // this is the
         def compare(a: (Key, Int), b: (Key, Int)): Int = {           // PriorityQueue
           if (b._2 < a._2) -1 else if (b._2 > a._2) 1                // ordering of
           else { if (b._1.x < a._1.x) -1 else if (b._1.x > a._1.x) 1 // of (Key, Int)
                     else { if (b._1.y < a._1.y) -1 else if (b._1.y > a._1.y) 1 else 0 }
           } } } )

    dist(source) = 0                       //  create vertex priority queue Q                              
    for (x <- 0 until w; y <- 0 until h) { //  for each vertex v in Graph:          
        val v = Key(x, y)
        if (v != source) {                 //      if v ≠ source                    
           dist(v) = 100000000             //          dist[v] ← INFINITY           
        }                                  // 
    }                                      //  create vertex priority queue Q                                       
    Q.enqueue ((source, 0))                //  Q.add_with_priority(v, dist[v])                                     
    dist(source) = 0                       //  dist[source] ← 0   // Initialization
  
    while (Q.nonEmpty) {                   //  while Q is not empty:                  
        val (u, k) = Q.dequeue()           //      (u, k)  ← Delete-Min Q

        if (k == dist(u)) {                //      if (k == dist[u]) then:
           for ((l, v) <- Graph(u)) {      //      for each neighbor v of u:          
               val alt = dist(u) + l       //          alt ← dist[u] + length(u, v)   
               if (alt < dist(v)) {        //          if alt < dist[v] then:
                   dist(v) = alt           //              dist[v] ← alt              
                   Q.enqueue((v, alt))     //              Insert Q (v, alt)
               }
           }
        }
    }
    dist(target)          // just the target len  //      return dist, prev
  }// Dijkstra

  val c = f.getLines().toArray.map { x => x.map(_.toInt - 48).toArray }

  val w = c(0).size; var h = c.size // x and y dimensions

  def g(v : Key): List[(Int, Key)] = {
     val x = v.x; val y = v.y
     List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)).collect(
       {case (x, y) if (x >= 0 && x < w && y >= 0 && y < h) => (c(y)(x), Key(x, y))})}

  println("answer1=" + Dijkstra(g, Key(0, 0), Key(w - 1, h - 1), w, h))

  def g2(v : Key): List[(Int, Key)] = {
     def c5(x: Int, y: Int): Int = {
       val n = c(y % h)(x % w) + (x / h) + (y / h)
       if (n > 9) n - 9 else n
     }
     val x = v.x; val y = v.y
     List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)).collect(
       {case (x, y) if (x >= 0 && x < 5*w && y >= 0 && y < 5*h) => (c5(x,y), Key(x, y))})}

  println("answer2=" + Dijkstra(g2, Key(0, 0), Key(5*w - 1, 5*h - 1), 5*w, 5*h))
}
