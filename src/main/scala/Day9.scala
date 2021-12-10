object Day9 extends Problem[Int] {
  override def day: Int = 9

  case class Position(x: Int, y: Int)

  override def part1(input: Array[String]): Option[Int] = {
    val heights = input.map(_.toCharArray.map(_ - '0'))

    val withNeighbors = for {
      x <- heights(0).indices
      y <- heights.indices
    } yield (heights(y)(x), neighborHeights(Position(x, y), heights))

    withNeighbors.collect { case (num, neighbors) if neighbors.forall(num < _) => num + 1 }.sum
  }

  override def part2(input: Array[String]): Option[Int] = {
    val heights = input.map(_.toCharArray.map(_ - '0'))

    val withNeighbors = for {
      x <- heights(0).indices
      y <- heights.indices
    } yield Position(x, y) -> (heights(y)(x), neighborHeights(Position(x, y), heights))

    val lowPoints = withNeighbors.collect {
      case (key, (num, neighbors)) if neighbors.forall(num < _) => key
    }

    val basinSizes = lowPoints.map(basinSize(_, heights))

    basinSizes.toList.sortWith(_ > _).take(3).reduce(_ * _)
  }

  private def basinSize(position: Position, heights: Array[Array[Int]], visited: Set[Position] = Set.empty): Int = {
    def loop(position: Position, visited: Set[Position]): (Int, Set[Position]) = {
      val Position(x, y) = position
      val currHeight = heights(y)(x)
      // already visited this position or it's height is 9
      if (currHeight == 9 || visited.contains(position)) {
        (0, visited)
      }
      // sum the basin sizes of the positions neighbors and add 1 for this position
      else {
        val neighbors = neighborPositions(position, heights)

        // start with sum 0 and visited set including this position
        val start: (Int, Set[Position]) = (0, visited + position)
        val (neighborBasinSize, updatedVisited) = neighbors.foldLeft(start) { case ((totalSize, visited), neighbor) =>
          val (basinSize, updatedVisited) = loop(neighbor, visited)
          (totalSize + basinSize, updatedVisited)
        }

        (neighborBasinSize + 1, updatedVisited)
      }
    }

    val (size, _) = loop(position, Set.empty)
    size
  }

  private def neighborPositions(position: Position, heights: Array[Array[Int]]): List[Position] = {
    val Position(x, y) = position
    List(
      Position(x - 1, y),
      Position(x + 1, y),
      Position(x, y - 1),
      Position(x, y + 1)
    ).filter { case Position(x, y) => x >= 0 && x < heights(0).length && y >= 0 && y < heights.length }
  }

  private def neighborHeights(position: Position, heights: Array[Array[Int]]): List[Int] =
    neighborPositions(position, heights).map { case Position(x1, y1) => heights(y1)(x1) }
}
