object Day7 extends Problem[Int] {
  override def day: Int = 7

  override def part1(input: Array[String]): Option[Int] = {
    val ints = input(0).split(",").map(_.toInt)
    (ints.min to ints.max).map { x =>
      ints.foldLeft(0)((a, i) => a + math.abs(i - x))
    }.min
  }

  override def part2(input: Array[String]): Option[Int] = {
    val ints = input(0).split(",").map(_.toInt)
    (ints.min to ints.max).map { x =>
      ints.foldLeft(0)((a, i) => a + math.abs(i - x) * (math.abs(i - x) + 1) / 2)
    }.min
  }
}
