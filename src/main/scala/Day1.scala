object Day1 extends Problem {
  override def day: Int = 1

  override def part1(input: Array[String]): Option[Int] =
    input.map(_.toInt).sliding(2).count { case Array(a, b) => a < b }

  override def part2(input: Array[String]): Option[Int] =
    input
      .map(_.toInt)
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count { case Seq(a, b) => a < b }
}
