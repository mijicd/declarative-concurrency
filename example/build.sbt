addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

inThisBuild(
  List(
    scalaVersion := "2.12.10",
    organization := "com.github.mijicd"
  )
)

lazy val ZioVersion = "1.0.0-RC21-1"

lazy val example = project
  .in(file("."))
  .aggregate(transfer, pqueue)

lazy val transfer = project
  .in(file("transfer"))
  .settings(
    libraryDependencies += "dev.zio" %% "zio" % ZioVersion
  )

lazy val pqueue = project
  .in(file("pqueue"))
  .settings(
    libraryDependencies += "dev.zio" %% "zio" % ZioVersion
  )
