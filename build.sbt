name := "mdpeg"

lazy val commonSettings = Seq(
  organization := "org.mdpeg",
  version := "0.1.0",
  scalaVersion := "2.12.1",
  libraryDependencies ++= Seq()
)

lazy val mdpeg = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.1.4",
      "org.scalatest" %% "scalatest" % "3.0.1" % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
    )
  )

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .dependsOn(mdpeg)