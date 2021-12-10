object Day10 extends Problem[Long] {
  override def day: Int = 10

  enum Line {
    case Incomplete(stack: List[Char])
    case Corrupted(illegalChar: Char)
  }

  override def part1(input: Array[String]): Option[Long] = {
    val processed = input.map(_.toCharArray).map(processLine)

    processed
      .collect {
        case _: Line.Incomplete  => 0
        case Line.Corrupted(')') => 3
        case Line.Corrupted(']') => 57
        case Line.Corrupted('}') => 1197
        case Line.Corrupted('>') => 25137
      }
      .sum
      .toLong
  }

  override def part2(input: Array[String]): Option[Long] = {
    val processed = input.map(_.toCharArray).map(processLine)

    val incomplete = processed.collect { case Line.Incomplete(stack) => stack }

    val sums = incomplete.map(_.foldLeft(0L) { case (sum, char) =>
      sum * 5 + (char match {
        case '(' => 1
        case '[' => 2
        case '{' => 3
        case '<' => 4
      })
    })

    val sorted = sums.sorted
    sorted(sums.length / 2)
  }

  private def processLine(line: Array[Char]): Line =
    line.foldLeft(Line.Incomplete(List.empty)) {
      case (lc @ Line.Corrupted(c), _) => lc
      case (Line.Incomplete(stack), char) =>
        char match {
          case '(' => Line.Incomplete('(' :: stack)
          case '[' => Line.Incomplete('[' :: stack)
          case '{' => Line.Incomplete('{' :: stack)
          case '<' => Line.Incomplete('<' :: stack)

          case ')' => if (stack.head == '(') Line.Incomplete(stack.tail) else Line.Corrupted(')')
          case ']' => if (stack.head == '[') Line.Incomplete(stack.tail) else Line.Corrupted(']')
          case '}' => if (stack.head == '{') Line.Incomplete(stack.tail) else Line.Corrupted('}')
          case '>' => if (stack.head == '<') Line.Incomplete(stack.tail) else Line.Corrupted('>')
        }
    }
}
