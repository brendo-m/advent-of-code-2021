object Day14 extends Problem[Long] {
  override def day: Int = 14

  // brute force approach. actually applies the insertions on the string repeatedly and then counts characters
  override def part1(input: Array[String]): Option[Long] = {
    val (template, insertions) = readInput(input)

    val result = (0 to 9).foldLeft(template) { case (acc, _) =>
      step(acc, insertions)
    }

    // obtain a count per character, sort, diff the most and least frequent
    val counts = result.toCharArray.groupMapReduce(identity)(_ => 1)(_ + _)
    val sorted = counts.toList.sortBy(_._2)
    sorted.last._2 - sorted.head._2
  }

  private def step(template: String, insertions: Map[String, String]): String = {
    val insertIntoPairs: List[String] = template
      .sliding(2)
      .map { pair =>
        insertions
          .get(pair)
          .map(insert => pair(0) + insert + pair(1))
          .getOrElse(pair)
      }
      .toList

    // join the new list of strings back together, removing the 1st char of each
    // except for the first one, since the sliding window introduces duplicates
    insertIntoPairs.head + insertIntoPairs.tail.map(_.drop(1)).mkString
  }

  // key insight is that we can keep track of the number of pairs that get created just by looking
  // at the insertions. if we have 1 NN -> C then after substituting we will have 0 NN -> C, 1 NC -> B
  // and 1 CN -> C. we break down the template string into an initial count of pairs and continually
  // apply the insertions in this way and count at the end
  override def part2(input: Array[String]): Option[Long] = {
    val (template, insertions) = readInput(input)

    val listInsertions = insertions.keys

    val initial: Map[String, Count] = template
      .sliding(2)
      .map(_.mkString)
      .toList
      .groupMapReduce(identity)(_ => Count(0L, 1L))(Count.add)

    val applied = (0 to 39).foldLeft(initial) { case (acc, _) =>
      applyInsertions(acc, insertions)
    }

    // count the occurrences of the second character in each pair. ignore the first because the
    // sliding window duplicates them. this means the very first character in the input is
    // under counted by 1 so we add 1 back later
    val characterCounts: Map[String, Long] = applied
      .groupMapReduce(_._1.drop(1))(_._2.existing)(_ + _)

    val firstChar = template.head.toString
    val updatedFirst = characterCounts.updated(firstChar, characterCounts(firstChar) + 1)

    val sorted = updatedFirst.toList.sortBy(_._2)

    sorted.last._2 - sorted.head._2
  }

  // need to track existing and new separately since we apply all subs at the same time
  case class Count(`new`: Long, existing: Long)

  object Count {
    def add(a: Count, b: Count): Count = Count(a.`new` + b.`new`, a.existing + b.existing)
  }

  private def applyInsertions(initial: Map[String, Count], insertions: Map[String, String]): Map[String, Count] =
    insertions
      .foldLeft(initial) { case (acc, (pair, sub)) =>
        if !acc.contains(pair) then acc
        else {
          val count = acc(pair)

          val newPair1 = pair.head + sub
          val newPair2 = sub + pair.last

          val newPair1Count = acc.getOrElse(newPair1, Count(0L, 0L))
          val newPair2Count = acc.getOrElse(newPair2, Count(0L, 0L))

          acc
            .updated(pair, count.copy(existing = 0L))
            .updated(newPair1, newPair1Count.copy(`new` = newPair1Count.`new` + count.existing))
            .updated(newPair2, newPair2Count.copy(`new` = newPair2Count.`new` + count.existing))
        }
      }
      .view
      .mapValues(c => Count(0L, c.`new`)) // switch the new counts to existing counts now we are done
      .toMap

  case class Insertion(pair: String, insert: String)

  private def readInput(input: Array[String]): (String, Map[String, String]) = {
    val emptyLine = input.indexOf("")
    val (template, rawInsertion) = input.splitAt(emptyLine)

    val insertions = rawInsertion
      .drop(1)
      .map(_.split(" -> "))
      .map { case Array(pair, insert) =>
        pair -> insert
      }
      .toMap

    (template.head, insertions)
  }
}
