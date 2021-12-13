object Day13 extends Problem[Int] {
  override def day: Int = 13

  override def part1(input: Array[String]): Option[Int] = {
    val (paper, folds) = readInput(input)

    val first = folds(0)
    fold(paper, first).size
  }

  override def part2(input: Array[String]): Option[Int] = {
    val (paper, folds) = readInput(input)

    println("PART 2: Paper after all folds")
    println("===========")
    val afterFolds = folds.foldLeft(paper)(fold)

    printPaper(afterFolds)

    // Returning None here because we get the answer visually above
    None
  }

  case class Position(x: Int, y: Int)

  enum Instruction:
    case FoldX(x: Int)
    case FoldY(y: Int)

  private def readInput(input: Array[String]): (Set[Position], Array[Instruction]) = {
    val emptyLine = input.indexOf("")
    val (rawPaper, rawInstructions) = input.splitAt(emptyLine)
    val paper = rawPaper
      .map(_.split(','))
      .map { case Array(x, y) =>
        Position(x.toInt, y.toInt)
      }
      .toSet

    val instructions = rawInstructions.drop(1).map { i =>
      val importantPart = i.split(" ")(2)
      importantPart.split("=") match {
        case Array("x", x) => Instruction.FoldX(x.toInt)
        case Array("y", x) => Instruction.FoldY(x.toInt)
      }
    }

    (paper, instructions)
  }

  def fold(paper: Set[Position], instruction: Instruction): Set[Position] =
    instruction match {
      case Instruction.FoldX(x) =>
        paper.foldLeft(paper) { case (acc, position) =>
          if position.x > x then acc + position.copy(x = 2 * x - position.x) - position
          else acc
        }

      case Instruction.FoldY(y) =>
        paper.foldLeft(paper) { case (acc, position) =>
          if position.y > y then acc + position.copy(y = 2 * y - position.y) - position
          else acc
        }
    }

  private def printPaper(paper: Set[Position]): Unit = {
    val map = paper.groupBy(identity)
    val maxX = paper.maxBy(_.x).x
    val maxY = paper.maxBy(_.y).y

    for y <- 0 to maxY do
      for x <- 0 to maxX do if map.contains(Position(x, y)) then print("#") else print(".")
      println()
  }
}
