package lambdaconf

import zio._
import zio.clock._
import zio.console._
import zio.duration._
import zio.stm._

/**
 * Problem:
 *  - Alice has 1000$ on her account, while Bob has 0.
 *  - Bob asks Alice to loan him 10000$.
 *  - Alice receives 100$ every 10 milliseconds until the transfer completes.
 */
object Transfer extends App {
  type Account = TRef[Long]

  object Account {
    def make(initial: Long): UIO[Account] = TRef.makeCommit(initial)
  }

  def transfer(from: Account, to: Account, amount: Long): UIO[Any] =
    STM.atomically {
      for {
        currBalance <- from.get
        _           <- STM.check(currBalance >= amount)
        _           <- from.update(_ - amount)
        _           <- to.update(_ + amount)
      } yield ()
    }

  def credit(account: Account, amount: Long): UIO[Any] =
    account.update(_ + amount).commit

  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val program =
      for {
        alice        <- Account.make(1000L)
        bob          <- Account.make(0L)
        _            <- transfer(alice, bob, 10000L).race(credit(alice, 100L).repeat(Schedule.fixed(10.millis)))
        balanceAlice <- alice.get.commit
        balanceBob   <- bob.get.commit
        _            <- putStrLn(s"Alice has $balanceAlice usd")
        _            <- putStrLn(s"Bob has $balanceBob usd")
      } yield ()

    program.exitCode
  }
}
