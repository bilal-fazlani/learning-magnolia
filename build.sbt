val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "magnoliaLearning",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      Libs.magnolia,
      Libs.rainbowcli,
    )
  )
