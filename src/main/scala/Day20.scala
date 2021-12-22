import scala.util.chaining.*

object Day20 extends Problem[Int]:
  override def day: Int = 20

  override def part1(input: Array[String]): Option[Int] =
    val (algorithm, image) = readInput(input)

    val (enhancedTwice, _) = image
      .pipe(
        enhance(_, algorithm, 0)
          .pipe { case (img, default) => enhance(img, algorithm, default) })

    enhancedTwice.positions.values.count(_ == 1)

  override def part2(input: Array[String]): Option[Int] =
    val (algorithm, image) = readInput(input)

    val (enhanced, _) = (0 to 49).foldLeft((image, 0)) { case ((enhanced, default), _) =>
      enhance(enhanced, algorithm, default)
    }

    enhanced.positions.values.count(_ == 1)

  private def enhance(inputImage: Image, algorithm: Algorithm, default: Int): (Image, Int) =
    val outputMin = Position(inputImage.min.x - 2, inputImage.min.y - 2)
    val outputMax = Position(inputImage.max.x + 2, inputImage.max.y + 2)
    val outputPositions = for
      y <- outputMin.y to outputMax.y
      x <- outputMin.x to outputMax.x
    yield
      val position = Position(x, y)
      val pixelValue = inputImage.pixelValue(position, default)
      position -> algorithm(pixelValue)

    val newDefault = if default == 0 then algorithm(0) else algorithm(511)

    (Image(outputPositions.toMap, outputMin, outputMax), newDefault)

  case class Position(x: Int, y: Int)

  type Bit = Int
  type Algorithm = Array[Bit]

  case class Image(positions: Map[Position, Bit], min: Position, max: Position):
    def neighbors(position: Position): Array[Position] =
      val Position(x, y) = position
      val nbs = for
        y1 <- y - 1 to y + 1
        x1 <- x - 1 to x + 1
      yield Position(x1, y1) // out of bounds allowed
      nbs.toArray

    def pixelValue(position: Position, default: Int): Int =
      neighbors(position)
        .map(p => positions.getOrElse(p, default))
        .pipe(binToDec)

  private def readInput(input: Array[String]): (Algorithm, Image) =
    val algorithm: Array[Bit] = input(0).toCharArray.map(toBit)
    val imageBits: Array[Array[Bit]] = input.drop(2).map(_.toCharArray.map(toBit))

    val positions = for
      x <- imageBits(0).indices
      y <- imageBits.indices
    yield Position(x, y) -> imageBits(y)(x)

    val image = Image(positions.toMap, Position(0, 0), Position(imageBits(0).length - 1, imageBits.length - 1))

    (algorithm, image)

  private def toBit(char: Char): Bit = if char == '#' then 1 else 0

  private def binToDec(bits: Array[Int]): Int =
    bits.foldLeft(0) { case (a, b) =>
      (a << 1) + b
    }
