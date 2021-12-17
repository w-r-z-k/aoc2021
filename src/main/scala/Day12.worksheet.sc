  import scala.io.Source

  val f = Source.fromFile("../../../data/Day12.example.txt")
  //val f = Source.fromFile("../../../data/Day12.txt")

  val input = f.getLines().toArray

  input.foreach(println)

val ggg = (for(l <- input) yield {
  val a = l.split("-")
  List((a(0), a(1)), (a(1), a(0)))
}).flatten.groupBy(_._1).map(v => (v._1, v._2.map(_._2)))

//  class Graph[T] {
//  type Vertex = T
//  type GraphMap = Map[Vertex,List[Vertex]]
//  var g:GraphMap = Map()
//
//  def BFS(start: Vertex): List[List[Vertex]] = {
//
//    def BFS0(elems: List[Vertex],visited: List[List[Vertex]]): List[List[Vertex]] = {
//      val newNeighbors = elems.flatMap(g(_)).filterNot(visited.flatten.contains).distinct
//      if (newNeighbors.isEmpty)
//        visited
//      else
//        BFS0(newNeighbors, newNeighbors :: visited)
//    }
//
//    BFS0(List(start),List(List(start))).reverse
//  }
//
//  def DFS(start: Vertex): List[Vertex] = {
//
//    def DFS0(v: Vertex, visited: List[Vertex]): List[Vertex] = {
//      if (visited.contains(v))
//        visited
//      else {
//        val neighbours:List[Vertex] = g(v) filterNot visited.contains
//        neighbours.foldLeft(v :: visited)((b,a) => DFS0(a,b))
//      }
//    }
//    DFS0(start,List()).reverse 
//  }
//}
//
//var sGraph = new Graph[String]
//
//sGraph.g = Map("Apple" -> List ("Banana","Pear","Grape"), "Banana" -> List("Apple","Plum"), "Pear" -> List("Apple","Plum"), "Grape" -> List("Apple","Plum"), "Plum" -> List ("Banana","Pear","Grape"))
//
//sGraph.BFS("Apple")
//
////start-A
////start-b        start     
////A-c            /   \     
////A-b        c--A-----b--d 
////b-d            \   /     
////A-end           end      
////b-end
//
//var z = new Graph[String]

val g = Map("start" -> List("A","b"),
          "A"     -> List("c","b","end"),
          "b"     -> List("d","end"),
          "c"     -> List("A"),
          "d"     -> List("b"),
          "end"   -> List("A","b")
)
g

//z.BFS("start")
//z.DFS("start")

def traverse(s: String, paths: List[List[String]]): List[List[String]] = {
  val cp = paths.head

  println(f"s=$s #paths=${paths.size}")
  if (s != "end") {
    g(s).foreach{s => print(f"$s ") };println()
     (for (n <- g(s) if(!cp.contains(n))) yield {
        println(f"n=$n")
        traverse(n, ((cp :+ s) +: paths.tail)) // ()+"start"
        //traverse("A", (List(List("start"))))
           //traverse("c", (List(List("start","A"))))
           //traverse("b", (List(List("start","A"))))
              //traverse("d",   (List(List("start","A","b"))))
              //traverse("end", (List(List("start","A","b"))))
                //(List(List("start","A","b","end"))))
           //traverse("end", (List(List("start","A"))))
                //(List(List("start","A","end"))))
        //traverse("b", (List(List("start"))))
           //traverse("d", (List(List("start","b"))))
           //traverse("end", (List(List("start","b"))))
              //(List(List("start","b","end"))))
     }).flatten
              //(List(List("start","A","b","end"))))
              //(List(List("start","A","end"))))
              //(List(List("start","b","end"))))
  } else
     (cp :+ s) +: paths.tail
}

val v = traverse("start", List(List()))
v.size
v.head
v.tail.head

val p = List(
List(List("start","A","b","end")),
List(List("start","A","end")),
List(List("start","b","end"))
)
p.flatten

"[A-Z].*".r.matches("Apple")


val m = List("start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end")
m.map(_.split("-")).flatten.distinct

for(l <- m; ll <- l.split("-")) yield(ll)

val list1 = for(l <- m) yield {
  val a = l.split("-")
  List((a(0), a(1)), (a(1), a(0)))
}
list1.flatten

//val list: List[(String, String)] = List(("a","b"),("c","d"),("a","f"))
//
val h = list1.flatten.groupBy(_._1).map(v => (v._1, v._2.map(_._2)))
h.head
h.tail.head
h.tail.tail.head
h.tail.tail.tail.head
h.tail.tail.tail.tail.head
h.tail.tail.tail.tail.tail.head
h.tail.tail.tail.tail.tail.head


val rr = List("A", "a", "a", "X", "c")
val rrr = rr.filter{x => !("[A-Z].*".r.matches(x))}.groupBy(identity).view.mapValues(_.size).toMap
rrr.values.toList.contains{x:Int => x > 1}

rr.filter{x => !("[A-Z].*".r.matches(x))}.groupBy(identity).view.mapValues(_.size).toMap.values.toList.contains{x:Int => x > 1}
val nn = rr.filter{x => !("[A-Z].*".r.matches(x))}.groupBy(identity).view.mapValues(_.size).toMap.values.toList
nn
nn.exists{x:Int => x > 1}
