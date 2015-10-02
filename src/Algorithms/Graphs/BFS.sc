import scala.collection.mutable
case class Graph(var value: Int) {
  var neighbours: List[Graph] = List.empty
  def addNeighbours(neighbour: Graph*) =
    neighbour.foreach(g => neighbours = neighbours :+ g)
}

val list = prepareListOfNodes()

def BFS(list: List[Graph], i: Graph): List[Graph] = {
  val queue = mutable.Queue[Graph]()        //create queue
  list.foreach(_.value = Integer.MAX_VALUE) // set all nodes to MAX
  i.value = 0                               //set i NODE to 0
  queue.enqueue(i)                          //add i NODE to QUEUE
  while (queue.nonEmpty) {
    val j = queue.dequeue()
    j.neighbours.foreach { n =>             // foreach neighbors of node on queue
      if (n.value == Integer.MAX_VALUE) {   // if neighbor value = MAX
        n.value = j.value + 1                 // set neighbor val to NODE + 1
        queue.enqueue(n)                      // and add neighbor to queue
      }
    }
  }
  list
}
def computeDistanceDistribution(nodes: List[Graph]) = {
  val map = collection.mutable.Map() ++ list.indices.map(_ -> 0).toMap
  list.foreach{ g =>
    BFS(list, g).foreach{graph =>
      map(graph.value) = map(graph.value) + 1
    }
  }
  map
}

println("BFS: ")
BFS(list, list.head)
println("computeDistanceDistribution: ")
computeDistanceDistribution(list)


def prepareListOfNodes(): List[Graph] = {
  val g0 = Graph(0)
  val g1 = Graph(1)
  val g2 = Graph(2)
  val g3 = Graph(3)
  val g4 = Graph(4)
  val g5 = Graph(5)
  g0.addNeighbours(g1, g3, g4)
  g1.addNeighbours(g0, g2, g3)
  g2.addNeighbours(g1, g4)
  g3.addNeighbours(g1, g0)
  g4.addNeighbours(g0, g2, g5)
  g5.addNeighbours(g4)
  List(g0, g1, g2, g3, g4, g5)
}

