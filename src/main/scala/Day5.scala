object Day5 extends Problem {
  override def day: Int = 5

  override def part1(input: Array[String]): Option[Int] =
    countPoints(
      input
        .map(Line.deserialize)
        .filter(l => Line.isVertical(l) || Line.isHorizontal(l)))

  override def part2(input: Array[String]): Option[Int] =
    countPoints(
      input
        .map(Line.deserialize))

  private def countPoints(lines: Array[Line]): Int =
    lines
      .flatMap(Line.pointsCovered)
      .foldLeft(Map.empty[Point, Int]) { case (acc, point) =>
        val count = acc.getOrElse(point, 0)
        acc.updated(point, count + 1)
      }
      .count { case (_, count) => count > 1 }
}

case class Point(x: Int, y: Int)

object Point {
  def deserialize(str: String): Point = {
    val Array(x, y) = str.split(",").map(_.toInt)
    Point(x, y)
  }
}

case class Line(start: Point, end: Point)

object Line {
  def deserialize(str: String): Line = {
    val Array(start, end) = str.split(" -> ")
    Line(Point.deserialize(start), Point.deserialize(end))
  }

  def pointsCovered(line: Line): Seq[Point] =
    if (isVertical(line) || isHorizontal(line)) {
      for {
        x <- xRange(line)
        y <- yRange(line)
      } yield Point(x, y)
    } else {
      xRange(line).zip(yRange(line)).map(Point.apply)
    }

  def isVertical(line: Line): Boolean = line.start.x == line.end.x

  def isHorizontal(line: Line): Boolean = line.start.y == line.end.y

  def xRange(line: Line): List[Int] = {
    val start = line.start.x
    val end = line.end.x
    if (start < end) (start to end).toList else (end to start).toList.reverse
  }

  def yRange(line: Line): List[Int] = {
    val start = line.start.y
    val end = line.end.y
    if (start < end) (start to end).toList else (end to start).toList.reverse
  }
}
