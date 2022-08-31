import sbt._ 

object Libs {
  //magnolia
  lazy val magnolia = "com.softwaremill.magnolia1_3" %% "magnolia" % "1.1.5"
  //rainbowcli
  lazy val rainbowcli = "com.bilal-fazlani" %% "rainbowcli" % "3.0.1"
  //pact
  lazy val pactJvm = "au.com.dius.pact.consumer" % "junit5" % "4.3.14"
}