name := "mdpeg"
version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.1.4",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)