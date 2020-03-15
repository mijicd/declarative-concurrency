package example

import zio._
import zio.clock._
import zio.console._
import zio.stm._

object Example extends App {
  class PriorityQueue[A] private (minPriority: TRef[Int], map: TMap[Int, TQueue[A]]) {
    def offer(a: A, priority: Int): STM[Nothing, Unit] =
      for {
        queueM <- map.get(priority)
        queue  <- queueM.fold(TQueue.unbounded[A])(STM.succeed(_))
        _      <- queue.offer(a)
        _      <- map.put(priority, queue)
        _      <- minPriority.update(current => if (current <= priority) current else priority)
      } yield ()

    def take: STM[Nothing, A] = {
      def takeFrom(currentMin: Int)(queue: TQueue[A]): STM[Nothing, A] =
        queue.take <* STM.whenM(queue.isEmpty)(increasePriority(currentMin))

      def increasePriority(currentMin: Int): STM[Nothing, Unit] =
        for {
          priorities <- map.delete(currentMin) *> map.keys
          newMin     = priorities.sorted.headOption.getOrElse(Int.MaxValue)
          _          <- minPriority.set(newMin)
        } yield ()

      for {
        current <- minPriority.get
        queueM  <- map.get(current)
        a       <- queueM.map(takeFrom(current)).getOrElse(STM.retry)
      } yield a
    }
  }

  object PriorityQueue {
    def make[A]: STM[Nothing, PriorityQueue[A]] =
      STM.mapN(TRef.make(Int.MaxValue), TMap.empty[Int, TQueue[A]])(new PriorityQueue(_, _))
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val program =
      for {
        _     <- putStrLn("Enter any key to exit...")
        queue <- PriorityQueue.make[String].commit
        _     <- makeOffers(queue, 3)
        _     <- makeOffers(queue, 0)
        _     <- queue.take.commit.flatMap(putStrLn(_)).forever.fork
        _     <- getStrLn
      } yield 0

    program.fold(_ => 1, _ => 0)
  }

  private def makeOffers(queue: PriorityQueue[String], priority: Int): UIO[Unit] =
    ZIO.foreach_(0 to 100)(i => queue.offer(s"Offer: $i with priority $priority", priority).commit)
}
