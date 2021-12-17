import scala.annotation.tailrec

object Day16 extends Problem[Long] {
  override def day: Int = 16

  override def part1(input: Array[String]): Option[Long] = {
    val bits = input(0).toCharArray.flatMap(hexToBin)

    val (packet, _) = parsePacket(bits)

    versionSum(packet).toLong
  }

  override def part2(input: Array[String]): Option[Long] = {
    val bits = input(0).toCharArray.flatMap(hexToBin)

    val (packet, _) = parsePacket(bits)

    evaluate(packet)
  }

  def versionSum(packet: Packet): Int = packet match {
    case Packet.Literal(version, _)           => version
    case Packet.Operator(version, _, packets) => version + packets.map(versionSum).sum
  }

  def evaluate(packet: Packet): Long = packet match {
    case Packet.Literal(_, num) => num
    case Packet.Operator(_, opType, packets) =>
      opType match {
        case OperatorType.Sum     => packets.map(evaluate).sum
        case OperatorType.Product => packets.map(evaluate).product
        case OperatorType.Min     => packets.map(evaluate).min
        case OperatorType.Max     => packets.map(evaluate).max
        case OperatorType.GT      => packets.map(evaluate).reduce((a, b) => if a > b then 1L else 0L)
        case OperatorType.LT      => packets.map(evaluate).reduce((a, b) => if a < b then 1L else 0L)
        case OperatorType.EQ      => packets.map(evaluate).reduce((a, b) => if a == b then 1L else 0L)
      }
  }

  enum OperatorType:
    case Sum
    case Product
    case Min
    case Max
    case GT
    case LT
    case EQ

  object OperatorType {
    def fromInt(i: Int): OperatorType = i match {
      case 0 => OperatorType.Sum
      case 1 => OperatorType.Product
      case 2 => OperatorType.Min
      case 3 => OperatorType.Max
      case 5 => OperatorType.GT
      case 6 => OperatorType.LT
      case 7 => OperatorType.EQ
    }
  }

  enum Packet(version: Int):
    case Literal(version: Int, num: Long) extends Packet(version)
    case Operator(version: Int, operator: OperatorType, packets: List[Packet]) extends Packet(version)

  def parsePacket(bits: Array[Int]): (Packet, Array[Int]) = {
    val (version, rest) = bits.splitAt(3)
    val versionDec = binToDec(version).toInt

    val (id, rest2) = rest.splitAt(3)
    val idDec = binToDec(id).toInt

    if idDec == 4 then parseLiteral(versionDec, rest2)
    else parseOperator(versionDec, OperatorType.fromInt(idDec), rest2)
  }

  private def parseLiteral(version: Int, bits: Array[Int]): (Packet.Literal, Array[Int]) = {
    val lastGroup = bits.zipWithIndex.indexWhere { case (b, i) => b == 0 && i % 5 == 0 }
    val (groups, rest) = bits.splitAt(lastGroup + 5)
    val converted = groups.grouped(5).flatMap(_.drop(1)).toArray

    (Packet.Literal(version, binToDec(converted)), rest)
  }

  private def parseOperator(
      version: Int,
      operatorType: OperatorType,
      bits: Array[Int]): (Packet.Operator, Array[Int]) = {
    val (lengthTypeId, rest) = bits.splitAt(1)

    val (packets, remaining) =
      if lengthTypeId.head == 0 then parseLengthType0(rest)
      else parseLengthType1(rest)

    (Packet.Operator(version, operatorType, packets), remaining)
  }

  private def parseLengthType0(bits: Array[Int]): (List[Packet], Array[Int]) = {
    val (length, rest) = bits.splitAt(15)
    val lengthDec = binToDec(length)
    val (toProcess, toDiscard) = rest.splitAt(lengthDec.toInt)

    @tailrec
    def loop(bs: Array[Int], acc: List[Packet]): List[Packet] =
      if (bs.length < 6) acc
      else {
        val (packet, remaining) = parsePacket(bs)
        loop(remaining, packet :: acc)
      }

    val packets = loop(toProcess, List.empty)

    // we built up list of packets in reverse
    (packets.reverse, toDiscard)
  }

  private def parseLengthType1(bits: Array[Int]): (List[Packet], Array[Int]) = {
    val (numPackets, rest) = bits.splitAt(11)
    val numPacketsDec = binToDec(numPackets)

    val (packets, rest2) = (0 until numPacketsDec.toInt).foldLeft((List.empty[Packet], rest)) {
      case ((packets, packetBits), _) =>
        val (packet, rest2) = parsePacket(packetBits)
        (packet :: packets, rest2)
    }

    // we built up list of packets in reverse
    (packets.reverse, rest2)
  }

  private def hexToBin(char: Char): Array[Int] = char match {
    case '0' => Array(0, 0, 0, 0)
    case '1' => Array(0, 0, 0, 1)
    case '2' => Array(0, 0, 1, 0)
    case '3' => Array(0, 0, 1, 1)
    case '4' => Array(0, 1, 0, 0)
    case '5' => Array(0, 1, 0, 1)
    case '6' => Array(0, 1, 1, 0)
    case '7' => Array(0, 1, 1, 1)
    case '8' => Array(1, 0, 0, 0)
    case '9' => Array(1, 0, 0, 1)
    case 'A' => Array(1, 0, 1, 0)
    case 'B' => Array(1, 0, 1, 1)
    case 'C' => Array(1, 1, 0, 0)
    case 'D' => Array(1, 1, 0, 1)
    case 'E' => Array(1, 1, 1, 0)
    case 'F' => Array(1, 1, 1, 1)
  }

  private def binToDec(bits: Array[Int]): Long =
    bits.foldLeft(0L) { case (a, b) =>
      (a << 1) + b
    }
}
