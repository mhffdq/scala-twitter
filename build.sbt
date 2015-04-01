name := "scala-twitter"

version := "1.0-SNAPSHOT"

resolvers ++= Seq(
  "repo.novus rels" at "http://repo.novus.com/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.mongodb" %% "casbah" % "2.7.3",
  "com.novus" %% "salat" % "1.9.9",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5",
  "org.json4s" %% "json4s-native" % "3.2.11",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test"
)

unmanagedBase := baseDirectory.value / "../../jar"

