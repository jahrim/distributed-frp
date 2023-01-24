ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "distributed-frp",
    libraryDependencies ++= Seq(
      "nz.sodium" % "sodium" % "1.2.0",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test
    )
  )
