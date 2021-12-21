import scala.util.chaining.*

object Day21 extends Problem[Long]:
  override def day: Int = 21

  override def part1(input: Array[String]): Option[Long] =
    val (player1, player2) = readInput(input)

    val rolls = LazyList.iterate(1)(i => i % 100 + 1)

    case class Game(p1: Player, p2: Player, rolls: LazyList[Int], numRolls: Int):
      def loser: Player = if p1.points <= p2.points then p1 else p2

    def play(game: Game): Game =
      if game.p1.points >= 1000 || game.p2.points >= 1000 then game
      else
        val Game(p1, p2, rolls, numRolls) = game
        val nextRolls = rolls.take(3).toList
        val newP1 = p1.advance(nextRolls.sum)
        val newGame = Game(p2, newP1, rolls.drop(3), numRolls + 3)
        play(newGame)

    val finish = play(Game(player1, player2, rolls, 0))
    finish.numRolls * finish.loser.points

  // takes 2 mins. I'm sure it could be heavily optimized, might come back to it
  override def part2(input: Array[String]): Option[Long] =
    val (player1, player2) = readInput(input)

    case class Game(p1: Player, p2: Player)
    case class Result(p1Wins: Long, p2Wins: Long)
    object Result:
      def winner(player: Player): Result =
        if player.id == 0 then Result(1, 0) else Result(0, 1)

    def play(game: Game, cache: Map[Game, Result] = Map.empty): (Result, Map[Game, Result]) =
      if cache.contains(game) then (cache(game), cache)
      else if game.p1.points >= 21 then Result.winner(game.p1).pipe(result => (result, cache + (game -> result)))
      else if game.p2.points >= 21 then Result.winner(game.p2).pipe(result => (result, cache + (game -> result)))
      else
        val newP1s =
          for
            i <- 1 to 3
            j <- 1 to 3
            k <- 1 to 3
          yield game.p1.advance(i + j + k)

        val newGames = newP1s
          .map(Game(game.p2, _))
          .groupMapReduce(identity)(_ => 1)(_ + _)
          .toList

        val result = newGames.foldLeft((Result(0, 0), cache)) { case ((totalResult, totalCache), (newGame, count)) =>
          val (gameResult, newCache) = play(newGame, totalCache)
          val newResult =
            Result(totalResult.p1Wins + gameResult.p1Wins * count, totalResult.p2Wins + gameResult.p2Wins * count)
          (newResult, newCache)
        }
        result

    val (result, _) = play(Game(player1, player2))
    math.min(result.p1Wins, result.p2Wins)

  case class Player(id: Int, position: Int, points: Int):
    def advance(num: Int): Player =
      val newPosition = (position + num).pipe(i => i - 10 * ((i - 1) / 10))
      val newPoints = points + newPosition
      Player(id, newPosition, newPoints)

  private def readInput(input: Array[String]): (Player, Player) =
    input
      .map(_.reverse.toCharArray.takeWhile(_ != ' ').reverse.mkString.toInt)
      .zipWithIndex
      .map { case (pos, idx) => Player(idx, pos, 0) }
      .pipe { case Array(p1, p2) =>
        (p1, p2)
      }
