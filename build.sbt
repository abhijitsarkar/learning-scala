lazy val commonSettings = Seq(
  organization := "name.abhijitsarkar.scala",
  version := "1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "learning-scala",
    libraryDependencies ++= Seq(
      "io.spray" % "spray-json_2.11" % "1.3.2",
      "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"
    ),
    scalacOptions ++= Seq("-unchecked", "-deprecation")
  )
