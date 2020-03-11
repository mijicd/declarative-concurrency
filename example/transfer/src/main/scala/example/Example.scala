package example

import zio._
import zio.clock._
import zio.duration._
import zio.stm._

object Example extends App {
  type Account = TRef[Long]

  object Account {
    def make(n: Long): UIO[Account] = TRef.makeCommit(n)
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    for {
      alice <- Account.make(1000L)
      bob   <- Account.make(0)
      _     <- refund(alice).race(transfer(alice, bob))
      av    <- alice.get.commit
      bv    <- bob.get.commit
      _     <- console.putStrLn(s"Alice has $av, while Bob has $bv euros.")
    } yield 0

  private def refund(account: Account): URIO[Clock, Int] = {
    val amount = 100L
    val policy = Schedule.fixed(10.millis)

    account.update(_ + amount).commit.repeat(policy)
  }

  private def transfer(from: Account, to: Account): UIO[Unit] =
    STM.atomically {
      val amount = 10000L

      for {
        current <- from.get
        _       <- STM.check(current >= amount)
        _       <- from.update(_ - amount)
        _       <- to.update(_ + amount)
      } yield ()
    }
}
