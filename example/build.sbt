addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

inThisBuild(
  List(
    scalaVersion := "2.12.10",
    version := "0.1.0",
    organization := "com.github.mijicd"
  )
)

lazy val example = project
  .in(file("."))
  .settings(
    libraryDependencies += "dev.zio" %% "zio" % "1.0.0-RC18"
  )
