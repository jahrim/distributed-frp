ThisBuild / scalaVersion := "3.2.2"

name := "distributed-frp"
inThisBuild(
  List(
    organization           := "io.github.cric96",
    homepage               := Some(url("https://github.com/cric96/distributed-frp")),
    licenses               := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    sonatypeCredentialHost := Sonatype.sonatype01,
    sonatypeRepository     := s"https://${Sonatype.sonatype01}/service/local",
    developers := List(
      Developer(
        "francescodente",
        "Francesco Dente",
        "francesco.dente@studio.unibo.it",
        url("https://github.com/francescodente")
      ),
      Developer(
        "cric96",
        "Gianluca Aguzzi",
        "gianluca.aguzzi@studio.unibo.it",
        url("https://cric96.github.io/")
      )
    )
  )
)
lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "nz.sodium" % "sodium" % "1.2.0",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test,
    ),
    parallelExecution := false
  )