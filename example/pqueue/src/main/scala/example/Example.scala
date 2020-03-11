package example

import zio._

object Example extends App {
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    console.putStrLn("Hello, World!").as(0)
}
