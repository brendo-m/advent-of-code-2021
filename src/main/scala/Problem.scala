import java.io.FileWriter
import java.net.HttpCookie
import java.nio.file.{Files, Paths}

trait Problem {
  def day: Int
  def part1(input: Array[String]): Option[Int]
  def part2(input: Array[String]): Option[Int]

  def main(args: Array[String]): Unit = {
    val input = fetchInput
    part1(input).foreach(answer => println(s"PART 1: ${answer}"))
    part2(input).foreach(answer => println(s"PART 1: ${answer}"))
  }

  def fetchInput: Array[String] = {
    val cacheDir = os.root / "tmp" / "aoc"
    val inputPath = cacheDir / s"$day.txt"

    os.makeDir.all(cacheDir)
    // TODO: make this work, the session value gets quoted and we get a 500 back
//    if (!os.exists(inputPath)) {
//      try os.write(
//        inputPath,
//        requests.get.stream(s"https://adventofcode.com/2020/day/$day/input", cookieValues = Map("session" -> session))
//      )
//      catch {
//        case x: requests.RequestFailedException => println(x.response)
//      }
//    }
    os.read.lines(inputPath).toArray
  }

  private def session = System.getenv("AOC_SESSION")

  implicit def intToOpt(i: Int): Option[Int] = Some(i)
}
