import scala.annotation.tailrec

object Day17 extends Problem[Int] {
  override def day: Int = 17

  override def part1(input: Array[String]): Option[Int] = {
    val target = readInput(input)

    // If the probe is launched from 0 at some velocity y it will eventually return back to 0 with
    // velocity -(y + 1) so we just need to find y_max such that -(y + 1) = target.minY. xVelocity is
    // irrelevant. Then finding the highest point is just the sum of numbers 1 to  y_max

    val yMax = -(target.minY + 1)
    yMax * (yMax + 1) / 2
  }

  override def part2(input: Array[String]): Option[Int] = {
    val target = readInput(input)

    // min x velocity is
    val xMin = LazyList
      .from(1)
      .dropWhile(x => x * (x + 1) < 2 * target.minX)
      .head

    // xMax is just the end of the target range
    val xMax = target.maxX

    // yMin is the end of the target range
    val yMin = target.minY

    // maxY is the same as in part 1
    val yMax = -(target.minY + 1)

    // brute force all the trajectories in these ranges
    val hittingTrajectories = for {
      x <- xMin to xMax
      y <- yMin to yMax
      if willHit(0, 0, x, y, target)
    } yield (x, y)

    hittingTrajectories.length
  }

  @tailrec
  private def willHit(x: Int, y: Int, xVelocity: Int, yVelocity: Int, target: Target): Boolean =
    if target.includes(x, y) then true
    else if x > target.maxX || y < target.minY then false
    else willHit(x + xVelocity, y + yVelocity, math.max(0, xVelocity - 1), yVelocity - 1, target)

  case class Target(minX: Int, maxX: Int, minY: Int, maxY: Int) {
    def includes(x: Int, y: Int): Boolean =
      minX <= x && x <= maxX && minY <= y && y <= maxY
  }

  private def readInput(input: Array[String]): Target = {
    val Array(x, y) = input(0).drop(13).split(", ")

    val Array(minX, maxX) = x.drop(2).split("\\.\\.")
    val Array(minY, maxY) = y.drop(2).split("\\.\\.")

    Target(minX.toInt, maxX.toInt, minY.toInt, maxY.toInt)
  }
}
