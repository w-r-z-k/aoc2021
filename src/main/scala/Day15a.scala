object Day15a extends App {

  import scala.collection.mutable.Map
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.PriorityQueue
  import scala.io.Source

  //val f = Source.fromFile("../../../data/Day15.example.txt")
  val f = Source.fromFile("../../../data/Day15.txt")

  // The Algorithm comes from Djikstra as detailed in the Wikipedea article 
  //
  //    https://en.wikipedia.org/wiki/Dijkstra's_algorithm
  //
  // under the section 'Using a Priority Queue'.  It also uses the
  // optimization detailed in the article:
  //
  //    https://cs.stackexchange.com/questions/118388/dijkstra-without-decrease-key
  //
  // and in doing so allows for the use of Scala's PriorityQueue which does not
  // have a decrease-key mechanism. The return of the actual shortest path is
  // roughed-in only because here we only need the shorted path length.
  
  type Key = (Int, Int)                // Pseudocode from Wikipedia w. Stackoverflow changes:

  def Dijkstra[Key](                   // function Dijkstra(Graph, source): 
      Graph: Map[Key, List[(Int, Key)]],
      source: Key,
      target: Key, // only want the target
  ): Int = {

    val dist = HashMap[Key, Int]()

    def QOrdering = new Ordering[(Key, Int)] {           // this is the
      def compare(a: (Key, Int), b: (Key, Int)): Int = { // PriorityQueue
        if (b._2 < a._2) -1 else if (b._2 > a._2) 1      // ordering of
        else { val (bx: Int, by: Int) = b._1             // of (Key, Int)
               val (ax: Int, ay: Int) = a._1
               if (bx < ax) -1 else if (bx > ax) 1 
                  else { if (by < ay) -1 else if (by > ay) 1 else 0 }
        } } }

    val Q = new PriorityQueue[(Key, Int)]()(QOrdering)

    dist(source) = 0                   //  create vertex priority queue Q                              
    for (v <- Graph.keys) {            //  for each vertex v in Graph:          
        if (v != source) {             //      if v ≠ source                    
           dist(v) = 100000000         //          dist[v] ← INFINITY           
           // prev(v) = UNDEFINED      //          prev[v] ← UNDEFINED          
        }                              // 
    }                                  //  create vertex priority queue Q                                       
    Q.enqueue ((source, 0))            //  Q.add_with_priority(v, dist[v])                                     
    dist(source) = 0                   //  dist[source] ← 0   // Initialization
  
    while (Q.nonEmpty) {               //  while Q is not empty:                  
        val (u, k) = Q.dequeue()       //      (u, k)  ← Delete-Min Q

        if (k == dist(u)) {            //      if (k == dist[u]) then:
           for ((l, v) <- Graph(u)) {  //      for each neighbor v of u:          
               val alt = dist(u) + l   //          alt ← dist[u] + length(u, v)   
               if (alt < dist(v)) {    //          if alt < dist[v] then:
                   dist(v) = alt       //              dist[v] ← alt              
                   // prev(v) = u      //              prev[v] ← u                
                   Q.enqueue((v, alt)) //              Insert Q (v, alt)
               }
           }
        }
    }
    dist(target)          // just the target len  //      return dist, prev
  }// Dijkstra

  val c = f.getLines().toArray.map { x => x.map(_.toInt - 48).toArray }

  var w = c(0).size; var h = c.size // x and y dimensions

  val g = Map[(Int, Int), List[(Int, (Int, Int))]]()

  g.addAll(for (y <- 0 until h; x <- 0 until w) yield {
               ((x, y), (if (x == w - 1) List() else List((c(y)(x + 1), (x + 1, y)))) ++
                        (if (y == h - 1) List() else List((c(y + 1)(x), (x, y + 1))))
          )})

  println("answer1=" + Dijkstra[(Int, Int)](g, (0, 0), (w - 1, h - 1)))

  val d = for (y <- 0 until h) yield {
    (for (i <- 0 until 5) yield {
      c(y).map { n => if (n + i > 9) n + i - 9 else n + i }
    }).flatten
  }.toArray

  val e = for (j <- 0 until 5 * h) yield {
    d(j % h).map { n => if (n + (j / h) > 9) n + (j / h) - 9 else n + (j / h) }
  }

  w = e(0).size; h = e.size // x and y dimensions

  val g2 = Map[(Int, Int), List[(Int, (Int, Int))]]()

  import scala.collection.mutable.ListBuffer

  g2.addAll(for (y <- 0 until h; x <- 0 until w) yield {
                val lb = new ListBuffer[(Int, (Int, Int))]
                if (x != 0)     lb += ((e(y)(x - 1), (x - 1, y)))
                if (x != w - 1) lb += ((e(y)(x + 1), (x + 1, y)))
                if (y != 0)     lb += ((e(y - 1)(x), (x, y - 1)))
                if (y != h - 1) lb += ((e(y + 1)(x), (x, y + 1)))
                ((x, y), lb.toList)})

  println("answer2=" + Dijkstra[(Int, Int)](g2, (0, 0), (w - 1, h - 1)))
}
