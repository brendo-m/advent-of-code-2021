object Day2 extends Problem[Int] {
  override def day: Int = 2

  override def part1(input: Array[String]): Option[Int] = {
    val resting = input.map(Direction.deserialize).foldLeft(Position(0, 0, 0)) { case (acc, i) =>
      i match {
        case Direction.Forward(amount) => acc.copy(horizontal = acc.horizontal + amount)
        case Direction.Up(amount) => acc.copy(depth = acc.depth - amount)
        case Direction.Down(amount) => acc.copy(depth = acc.depth + amount)
      }
    }
    resting.horizontal * resting.depth
  }

  override def part2(input: Array[String]): Option[Int] = {
    val resting = input.map(Direction.deserialize).foldLeft(Position(0, 0, 0)) { case (acc, i) =>
      i match {
        case Direction.Forward(amount) => acc.copy(horizontal = acc.horizontal + amount, depth = acc.depth + acc.aim * amount)
        case Direction.Up(amount) => acc.copy(aim = acc.aim - amount)
        case Direction.Down(amount) => acc.copy(aim = acc.aim + amount)
      }
    }
    resting.horizontal * resting.depth
  }
}

case class Position(horizontal: Int, depth: Int, aim: Int)

sealed trait Direction
object Direction {
  case class Forward(amount: Int) extends Direction
  case class Up(amount: Int) extends Direction
  case class Down(amount: Int) extends Direction

  def deserialize(str: String): Direction = str.split(" ") match {
    case Array("forward", amount) => Forward(amount.toInt)
    case Array("up", amount) => Up(amount.toInt)
    case Array("down", amount) => Down(amount.toInt)
  }
}