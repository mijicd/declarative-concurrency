import Dependencies._

inThisBuild(
  List(
    scalaVersion := "2.13.1",
    organization := "com.github.mijicd"
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies += "dev.zio" %% "zio" % "1.0.0-RC21-1"
  )
