import scala.annotation.tailrec

object Day15 extends Problem[Int] {
  override def day: Int = 15

  override def part1(input: Array[String]): Option[Int] = {
    val graph = Graph.deserialize(input, 1)

    val shortestPaths = djikstras(graph)

    shortestPaths(graph.end) match {
      case Weight.Visited(_, weight) => weight
      case Weight.Inf                => ???
    }
  }

  override def part2(input: Array[String]): Option[Int] = {
    val graph = Graph.deserialize(input, 5)

    val shortestPaths = djikstras(graph)

    shortestPaths(graph.end) match {
      case Weight.Visited(_, weight) => weight
      case Weight.Inf                => ???
    }
  }

  enum Weight:
    case Inf
    case Visited(from: Position, weight: Int)

  object Weight {
    extension (wtf: Weight) {
      def weight: Int = wtf match {
        case Weight.Inf                => Int.MaxValue
        case Weight.Visited(_, weight) => weight
      }
    }
  }

  private def djikstras(graph: Graph): Map[Position, Weight] = {
    @tailrec
    def loop(
        position: Position,
        weights: Map[Position, Weight],
        active: Set[Position],
        touched: Set[Position]): Map[Position, Weight] =
      if (active.isEmpty) weights
      else {
        val currWeight = weights(position).weight
        val neighbors = Graph.neighbors(position, graph).filter(active.contains)

        val (neighborWeights, newTouched) = neighbors.foldLeft((weights, touched)) {
          case ((neighborWeights, newTouched), neighbor) =>
            val neighborWeight = graph.weight(neighbor)

            val existingWeight = neighborWeights(neighbor)
            val newNeighborWeights = existingWeight match {
              case Weight.Inf =>
                neighborWeights.updated(neighbor, Weight.Visited(position, currWeight + neighborWeight))
              case Weight.Visited(_, existing) =>
                val newWeight = currWeight + neighborWeight
                if newWeight < existing then neighborWeights.updated(neighbor, Weight.Visited(position, newWeight))
                else neighborWeights
            }

            (newNeighborWeights, newTouched + neighbor)
        }

        // this would probably be a lot more efficient to do with a priorityqueue
        val lowestActive =
          if newTouched.nonEmpty then newTouched.minBy(n => Weight.weight(weights(n)))
          else active.head

        loop(lowestActive, neighborWeights, active - position, newTouched - position)
      }

    val initialWeights =
      graph.positions.keys
        .map(_ -> Weight.Inf)
        .toMap
        .updated(graph.start, Weight.Visited(graph.start, 0))

    loop(graph.start, initialWeights, graph.positions.keys.toSet, Set.empty)
  }

  case class Position(x: Int, y: Int)

  case class Graph(positions: Map[Position, Int], maxX: Int, maxY: Int) {
    val start: Position = Position(0, 0)
    val end: Position = Position(maxX, maxY)

    def weight(position: Position): Int = positions(position)
  }

  object Graph {
    def deserialize(input: Array[String], expandFactor: Int): Graph = {
      val arr = input.map(_.toCharArray.map(_ - '0'))
      val maxX = arr(0).length
      val maxY = arr.length

      val positions: Map[Position, Int] = (for {
        ex <- 0 until expandFactor
        ey <- 0 until expandFactor
        y <- arr.indices
        x <- arr(0).indices
      } yield {
        val actualX = x + maxX * ex
        val actualY = y + maxY * ey
        val position = Position(actualX, actualY)
        val energy = arr(y)(x) + actualX / maxX + actualY / maxY
        val adjustedEnergy = energy % 10 + (if energy > 9 then 1 else 0)
        position -> adjustedEnergy
      }).toMap

      Graph(positions, arr(0).length * expandFactor - 1, arr.length * expandFactor - 1)
    }

    def neighbors(position: Position, graph: Graph): List[Position] = {
      val Position(x, y) = position
      List(
        (x - 1, y),
        (x + 1, y),
        (x, y - 1),
        (x, y + 1)
      ).collect { case (x, y) if x >= 0 && x <= graph.maxX && y >= 0 && y <= graph.maxY => Position(x, y) }
    }
  }
}
