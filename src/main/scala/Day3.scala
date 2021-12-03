import scala.annotation.tailrec

object Day3 extends Problem {
  override def day: Int = 3

  override def part1(input: Array[String]): Option[Int] = {
    val ints = input.map(_.toCharArray.map(_ - '0'))

    val gamma = toDecimal(mostCommonBits(ints))
    val epsilon = toDecimal(leastCommonBits(ints))

    gamma * epsilon
  }

  override def part2(input: Array[String]): Option[Int] = {
    val ints = input.map(_.toCharArray.map(_ - '0'))

    val oxygen = toDecimal(filterByCriteria(ints, mostCommonBits))
    val co2 = toDecimal(filterByCriteria(ints, leastCommonBits))

    oxygen * co2
  }

  private def toDecimal(bits: Array[Int]): Int =
    bits.foldLeft(0) { case (acc, i) => (acc << 1) + i }

  private def leastCommonBits = mostCommonBits.andThen(_.map(1 - _))

  private def mostCommonBits(input: Array[Array[Int]]): Array[Int] = {
    val length = input.length
    val bits = input(0).length

    val abc = input
      .foldLeft(Array.fill(bits)(0))((a, b) => a.zip(b).map { case (a, b) => a + b })

    abc.map(x => if (x >= length.toDouble / 2) 1 else 0)
  }

  @tailrec
  private def filterByCriteria(
      input: Array[Array[Int]],
      bitCriteria: Array[Array[Int]] => Array[Int],
      bit: Int = 0): Array[Int] = {
    val criteria = bitCriteria(input)

    val filtered = input.filter(x => x(bit) == criteria(bit))
    filtered match {
      case Array(single) => single
      case _             => filterByCriteria(filtered, bitCriteria, bit + 1)
    }
  }
}
