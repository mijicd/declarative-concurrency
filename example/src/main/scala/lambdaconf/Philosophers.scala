package lambdaconf

import zio._
import zio.console._
import zio.stm._

object Philosophers extends App {

  case object Fork
  type Fork = Fork.type

  final case class Seat(left: TRef[Option[Fork]], right: TRef[Option[Fork]])

  final case class Table(seats: Chunk[Seat]) extends AnyVal

  def takeForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]]): USTM[(Fork, Fork)] =
    (left.get <*> right.get).flatMap {
      case (Some(l), Some(r)) => left.set(None) *> right.set(None).as((l, r))
      case _                  => STM.retry
    }

  def putForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]])(forks: (Fork, Fork)): USTM[Unit] =
    (left.get <*> right.get).flatMap {
      case (None, None) => left.set(Some(forks._1)) *> right.set(Some(forks._2))
      case _            => STM.retry
    }

  def setupTable(size: Int): UIO[Table] = {
    val makeFork = TRef.make[Option[Fork]](Some(Fork))

    STM.atomically {
      for {
        forks0 <- STM.foreach(0 to size)(_ => makeFork)
        forks  = forks0 ++ List(forks0(0))
        seats  = (forks zip forks.drop(1)).map { case (l, r) => Seat(l, r) }
      } yield Table(Chunk.fromIterable(seats))
    }
  }

  def eat(philosopher: Int, roundtable: Table): URIO[Console, Unit] = {
    val placement = roundtable.seats(philosopher)

    val left  = placement.left
    val right = placement.right

    for {
      forks <- takeForks(left, right).commit
      _     <- putStrLn(s"Philosopher $philosopher eating...")
      _     <- putForks(left, right)(forks).commit
      _     <- putStrLn(s"Philosopher $philosopher is done eating")
    } yield ()
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    val count = 10

    def eaters(table: Table): Iterable[URIO[Console, Unit]] =
      (1 to count).map(index => eat(index, table))

    for {
      table <- setupTable(count)
      fiber <- ZIO.forkAll(eaters(table))
      _     <- fiber.join
      _     <- putStrLn("All philosophers have eaten!")
    } yield ExitCode.success
  }
}
