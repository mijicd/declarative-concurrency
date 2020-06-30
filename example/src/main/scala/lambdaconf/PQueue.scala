package lambdaconf

import zio._
import zio.console._
import zio.stm._

object PQueue extends App {
  final class PriorityQueue[A](minPriority: TRef[Int], map: TMap[Int, TQueue[A]]) {
    def offer(a: A, priority: Int): USTM[Unit] =
      for {
        queueM <- map.get(priority)
        queue  <- queueM.fold(TQueue.unbounded[A])(STM.succeed(_))
        _      <- queue.offer(a)
        _      <- map.put(priority, queue)
        _      <- minPriority.update(curr => if (curr > priority) priority else curr)
      } yield ()

    def take: USTM[A] = {
      def increaseMin(curr: Int): USTM[Unit] =
        for {
          priorities <- map.delete(curr) *> map.keys
          newMin     = priorities.minOption.getOrElse(Int.MaxValue)
          _          <- minPriority.set(newMin)
        } yield ()

      for {
        curr   <- minPriority.get
        queueM <- map.get(curr)
        queue  <- queueM.fold(STM.retry: USTM[TQueue[A]])(STM.succeed(_))
        a      <- queue.take <* STM.whenM(queue.isEmpty)(increaseMin(curr))
      } yield a
    }
  }

  object PriorityQueue {
    def make[A]: USTM[PriorityQueue[A]] =
      for {
        ref <- TRef.make(Int.MaxValue)
        map <- TMap.empty[Int, TQueue[A]]
      } yield new PriorityQueue(ref, map)
  }

  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val program =
      for {
        _  <- putStrLn("Enter any key to exit...")
        pq <- PriorityQueue.make[String].commit
        _  <- makeOffers(pq, 3)
        _  <- makeOffers(pq, 0)
        _  <- pq.take.commit.flatMap(putStrLn(_)).forever.fork
        _  <- getStrLn
      } yield ()

    program.exitCode
  }

  private def makeOffers(pq: PriorityQueue[String], priority: Int): UIO[Unit] =
    ZIO.foreach_(0 to 10)(i => pq.offer(s"Offer: $i with priority $priority", priority).commit)
}
