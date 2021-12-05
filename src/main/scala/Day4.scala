object Day4 extends Problem {
  override def day: Int = 4

  override def part1(input: Array[String]): Option[Int] = {
    val (numbers, boards) = readInput(input)

    val start: board.Result = board.Result(boards, List.empty)
    val result = numbers.foldLeft(start) { case (result, number) =>
      result match {
        case won @ board.Result(_, winningBoard :: Nil) => won // shortcircuit after first winner
        case _ =>
          board.markBoards(result.incompleteBoards, number)
      }
    }

    result match {
      case board.Result(_, (winningBoard, winningNumber) :: Nil) =>
        board.sumUnmarked(winningBoard) * winningNumber
      case _ => throw new Exception("no winner")
    }
  }

  override def part2(input: Array[String]): Option[Int] = {
    val (numbers, boards) = readInput(input)

    val start: board.Result = board.Result(boards, List.empty)
    val result = numbers.foldLeft(start) { case (result, number) =>
      result match {
        case won @ board.Result(Nil, _) => won // shortcircuit after all winners
        case _ =>
          board.markBoards(result.incompleteBoards, number)
      }
    }

    result match {
      case board.Result(Nil, winners) =>
        // winning boards are inserted in reverse
        val (lastWinningBoard, lastWinningNumber) = winners.head
        board.sumUnmarked(lastWinningBoard) * lastWinningNumber
      case _ => throw new Exception("not all boards won")
    }
  }

  private def readInput(input: Array[String]): (Array[Int], List[board.Board]) = {
    val (numberLine, boardInputs) = input.splitAt(1)
    val numbers = numberLine.head.split(",").map(_.toInt)

    val boards = boardInputs
      .grouped(6)
      .map { lines =>
        // 1st line is empty so drop it. then split all the others on whitespace and convert numbers to Int
        val processedLines = lines.drop(1).map(_.trim.split("\\s+").map(_.toInt))
        // turn them into Boards
        board.initBoard(processedLines)
      }
      .toList

    (numbers, boards)
  }
}

object board {
  def initBoard(input: Array[Array[Int]]): Board =
    Board(input.reduce(_ concat _).map(Cell.Number.apply))

  def markBoards(boards: List[Board], number: Int): Result = {
    // start with a Continue with empty boards
    val start: Result = Result(List.empty, List.empty)
    boards.foldLeft(start) { case (result, checkingBoard) =>
      val (newBoard, winner) = markBoard(checkingBoard, number)
      if (winner) result.copy(winningBoards = (newBoard, number) :: result.winningBoards)
      else result.copy(incompleteBoards = newBoard :: result.incompleteBoards)
    }
  }

  def markBoard(board: Board, number: Int): (Board, Boolean) = {
    val cellIdx = board.cells.indexOf(Cell.Number(number))
    // number might not be on board
    if (cellIdx == -1) (board, false)
    else {
      val newBoard = board.copy(cells = board.cells.updated(cellIdx, Cell.Marked))
      (newBoard, checkBoard(newBoard, cellIdx))
    }
  }

  def checkBoard(board: Board, markedIdx: Int): Boolean = {
    val startRowIdx = (markedIdx / 5) * 5
    val rowMarked = (startRowIdx to startRowIdx + 4).forall(board.cells(_) == Cell.Marked)

    val column = markedIdx % 5
    val colMarked = (0 to 4).map(c => c * 5 + column).forall(board.cells(_) == Cell.Marked)

    rowMarked || colMarked
  }

  def sumUnmarked(board: Board): Int =
    board.cells.collect { case Cell.Number(num) => num }.sum

  case class Board(cells: Array[Cell])

  case class Result(incompleteBoards: List[Board], winningBoards: List[(Board, Int)])

  enum Cell {
    case Number(x: Int)
    case Marked

    override def toString: String = this match {
      case Number(x) => x.toString
      case Marked    => "x"
    }
  }
}
