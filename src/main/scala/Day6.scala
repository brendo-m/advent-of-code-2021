object Day6 extends Problem[Long] {
  override def day: Int = 6

  override def part1(input: Array[String]): Option[Long] = {
    val fish = input(0).split(",").map(_.toInt)

    // naive brute force approach
    (0 to 79)
      .foldLeft(fish) { case (acc, day) =>
        acc.flatMap {
          case 0 => Array(6, 8)
          case x => Array(x - 1)
        }
      }
      .length
      .toLong
  }

  override def part2(input: Array[String]): Option[Long] = {
    val fish = input(0).split(",").map(_.toInt)

    // much more efficient approach of just counting fish at different timer values
    val grouped: Map[Int, Long] = fish.groupBy(identity).view.mapValues(_.length.toLong).toMap
    val result = (0 to 255).foldLeft(grouped) { case (acc, day) =>
      val zeroFish = acc.getOrElse(0, 0L)

      (0 to 7)
        .foldLeft(acc) { case (newAcc, i) =>
          if (i == 6) newAcc.updated(i, acc.getOrElse(i + 1, 0L) + zeroFish)
          else newAcc.updated(i, acc.getOrElse(i + 1, 0L))
        }
        .updated(8, zeroFish)
    }
    result.values.sum
  }
}
