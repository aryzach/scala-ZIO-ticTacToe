package com.example
import zio._
import zio.console._
import zio.random._
import java.io.IOException
import scala.annotation.tailrec
import collection.immutable.SortedMap

case class State (board: Board, turn: Boolean, bot: BoolInput) {
  val winner: Option[Boolean] = board.winner
  val draw: Boolean = (this.winner == None) && (board.getEmpty.size == 0)
  def update(m: Move): State = 
    if (board.getEmpty contains m.position) 
      this.copy(board = board.update(m)(turn), turn = !turn) 
    else
      this.copy()
}

object State {
  def initialState(b: BoolInput): State = new State(Board(SortedMap()), true, b) {}
}

case class Board(b: SortedMap[Int, Boolean]) {
  val lines = List((0,1,2),(3,4,5),(6,7,8),(0,3,6),(1,4,7),(2,5,8),(0,4,8),(2,4,6))
  val indices = List(0,1,2,3,4,5,6,7,8)
  def getEmpty: List[Int] = indices.filter(x => !(b.keysIterator contains x))
  def update(m: Move)(turn: Boolean): Board = Board(b + (m.position -> turn))
  def winLine(tup: (Boolean,Boolean,Boolean)): Option[Boolean] = {
    val x = (tup._1 == tup._2 && tup._2 == tup._3) 
    if (x) Some(tup._1) else None
  }
  def checkLineWin(tup: (Int, Int, Int)): Option[Boolean] = 
    for {
      x <- b.get(tup._1)
      y <- b.get(tup._2)
      z <- b.get(tup._3)
      w <- winLine((x,y,z))
    } yield w
  val winner: Option[Boolean] = lines.map(checkLineWin(_)).flatten.headOption

  def stringify: String = {
    val chars = indices.map(i => 
        b.get(i) match {
          case Some(true)  => 'X'
          case Some(false) => 'O'
          case _           => i.toString 
        })

    s"""
    #  ${chars(0)} | ${chars(1)} | ${chars(2)} 
    # ___________
    #
    #  ${chars(3)} | ${chars(4)} | ${chars(5)} 
    # ___________
    #
    #  ${chars(6)} | ${chars(7)} | ${chars(8)} 
    """.stripMargin('#')
  }
}



abstract case class Move (position: Int) 
object Move {
  def make(s: String): Option[Move] = {
    s.toIntOption collect {
      case n if Set(0,1,2,3,4,5,6,7,8) contains n => new Move(n) {}
    }
  }
  def makeFromInt(i: Int): Move = new Move(i) {}
}

abstract case class BoolInput (b: Boolean) 
object BoolInput {
  def make(s: String): Option[BoolInput] =
    s.headOption collect {
      case 'y' => new BoolInput(true) {}
      case 'n'   => new BoolInput(false) {}
    }
}

sealed trait Result 
object Result {
  case object Valid extends Result 
  case object Invalid extends Result 
  case object Win extends Result 
  case object Draw extends Result 
}

object TicTacToe extends App {
  def displayState(s: State): ZIO[Console, Nothing, Unit] = {
    val turn = if (s.turn) "Player 1 turn" else "Player 2 turn"
    val board = s.board.stringify
    val string = s"""
    #${board}
    #${turn}
    """.stripMargin('#')

    putStrLn(string)
  }

  def getUserInput(message: String): ZIO[Console, IOException, String] = putStrLn(message).flatMap(_ => getStrLn)

  val getMove: ZIO[Console, IOException, Move] = getStrLn.flatMap(m => ZIO.fromOption(Move.make(m)) <> putStrLn("not a board position") *> getMove)

  val getBotChoice: ZIO[Console, IOException, BoolInput] = getBoolInput("play bot? y or n")("enter 'y' or 'n'") 
  val playAgain: ZIO[Console, IOException, BoolInput] = getBoolInput("play again? y or n")("enter 'y' or 'n'") 

  def getBoolInput(message: String)(error: String): ZIO[Console, IOException, BoolInput] = 
    for {
      _ <- putStrLn(message)
      i <- getStrLn 
      f <- ZIO.fromOption(BoolInput.make(i)) <> putStrLn(error) *> getBoolInput(message)(error)
    } yield f



  def getBotMove(s: State): ZIO[Random, Nothing, Move] = 
    nextIntBounded(s.board.getEmpty.length).flatMap(i => 
        ZIO.succeed(Move.makeFromInt(s.board.getEmpty(i))))



  def handleMove(m: Move)(s: State)(n: State): Result = {
    if (n.winner != None) Result.Win
    else if (n.draw) Result.Draw
    else if (s == n) Result.Invalid
    else Result.Valid
  }


  def gameLoop(s: State): ZIO[Console with Random, IOException , Unit] = {
    for {
      _ <- displayState(s)
      m <- if (s.bot.b && !s.turn) getBotMove(s) else getMove
      newState = s.update(m)
      a = handleMove(m)(s)(newState)
      _ <- a match {
        case Result.Invalid => putStrLn("invalid") *> gameLoop(s)
        case Result.Draw    => displayState(newState) *> putStrLn("draw") 
        case Result.Win     => displayState(newState) *> putStrLn("win")
        case Result.Valid   => gameLoop(newState)
      }
    } yield ()
  }

  val mainLoop: ZIO[Console with Random, IOException, Unit] = 
    for {
      bot    <- getBotChoice
      again  <- gameLoop(State.initialState(bot)) *> playAgain
      _      <- again.b match {
        case true => mainLoop
        case false => ZIO.succeed(()) 
      }
    } yield ()

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    mainLoop.exitCode
}






