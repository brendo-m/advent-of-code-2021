object Day8 extends Problem[Int] {
  override def day: Int = 8

  override def part1(input: Array[String]): Option[Int] = {
    val lines = input.map(Line.deserialize)
    lines
      .collect { case Line(_, outputs) =>
        outputs.filter(o => Set(2, 3, 4, 7).contains(o.size))
      }
      .map(_.length)
      .sum
  }

  override def part2(input: Array[String]): Option[Int] = {
    val lines = input.map(Line.deserialize)

    lines.map(solveLine).sum
  }

  def solveLine(line: Line): Int = {
    // collect the uniques into a map, 1,4,7,8
    val uniques = line.inputs.foldLeft(Map.empty[Int, Set[Char]]) { case (map, input) =>
      input.size match {
        case 2 => map.updated(1, input)
        case 3 => map.updated(7, input)
        case 4 => map.updated(4, input)
        case 7 => map.updated(8, input)
        case _ => map
      }
    }
    // do another pass using the fact that the remaining numbers can be deduced from the uniques
    val allNumbers = line.inputs.foldLeft(uniques) { case (map, input) =>
      input.size match {
        // numbers 0, 6 and 9 have 6 segments
        case 6 =>
          // 9 and only 9 has 4 segments in common with 4
          if (map(4).intersect(input).size == 4) {
            map.updated(9, input)
            // after removing 4, only 0 has 2 segments in common with 1
          } else if (map(1).intersect(input).size == 2) {
            map.updated(0, input)
            // otherwise must be 6
          } else {
            map.updated(6, input)
          }
        // numbers 2, 3 and 5 have 5 segments
        case 5 =>
          // 3 and only 3 has 2 segments in common with 1
          if (map(1).intersect(input).size == 2) {
            map.updated(3, input)
            // after removing 1, only 2 has 2 segments in common with 4. 5 has 3
          } else if (map(4).intersect(input).size == 2) {
            map.updated(2, input)
            // otherwise must be 5
          } else {
            map.updated(5, input)
          }
        case _ => map
      }
    }

    val outputDigits = line.outputs.map(o => allNumbers.collect { case (num, chars) if o == chars => num }.head)
    outputDigits.mkString("").toInt
  }

  case class Line(inputs: Array[Set[Char]], outputs: Array[Set[Char]])

  object Line {
    def deserialize(str: String): Line = {
      val Array(inputs, outputs) = str.split(" \\| ").map(_.split(" "))
      Line(inputs.map(_.toCharArray.toSet), outputs.map(_.toCharArray.toSet))
    }
  }
}
