ThisBuild / version := "0.2"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := " Algorithms.scala.Random",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )

