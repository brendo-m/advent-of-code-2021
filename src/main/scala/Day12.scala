object Day12 extends Problem[Int] {

  import Graph.*

  override def day: Int = 12

  override def part1(input: Array[String]): Option[Int] = {
    val graph = Graph.deserialize(input)

    val allPaths = findEnd(graph, (cave, visited) => visited.contains(cave))
    allPaths.length
  }

  override def part2(input: Array[String]): Option[Int] = {
    val graph = Graph.deserialize(input)

    val allPaths = findEnd(graph, (cave, visited) => visited.contains(cave) && visited.values.max == 2)
    allPaths.length
  }

  private def findEnd(graph: Graph, condition: (cave: Cave, visited: Map[Cave, Int]) => Boolean): List[Path] = {
    def loop(from: Cave, visited: Map[Cave, Int] = Map.empty): List[Path] =
      from match {
        case Cave.End => List(List(Cave.End))
        case Cave.Start if visited.contains(Cave.Start) => List.empty
        case cave: Cave if condition(from, visited) => List.empty
        case cave: Cave =>
          val updatedVisited = cave match {
            case _: Cave.Large => visited
            case _ =>
              val count = visited.getOrElse(cave, 0)
              visited.updated(cave, count + 1)
          }
          graph
            .neighbors(cave)
            .foldLeft(List.empty) { case (allPaths, neighbor) =>
              val newPaths = loop(neighbor, updatedVisited).map(from :: _)
              newPaths ::: allPaths
            }
      }

    loop(Cave.Start)
  }

  enum Cave {
    case Start
    case End
    case Large(s: String)
    case Small(s: String)
  }

  type Path = List[Cave]
  opaque type Graph = Map[Cave, List[Cave]]

  object Graph {
    val empty: Graph = Map.empty

    def deserialize(input: Array[String]): Graph =
      input.foldLeft(empty) { case (graph, connection) =>
        val Array(from, to) = connection.split("-").map {
          case "start" => Cave.Start
          case "end" => Cave.End
          case s if s.toLowerCase == s => Cave.Small(s)
          case s => Cave.Large(s)
        }
        val fromNeighbors = graph.getOrElse(from, List.empty)
        val toNeighbors = graph.getOrElse(to, List.empty)
        graph.updated(from, to :: fromNeighbors).updated(to, from :: toNeighbors)
      }

    extension (graph: Graph) {
      def neighbors(from: Cave): List[Cave] = graph.getOrElse(from, List.empty)
    }
  }
}
