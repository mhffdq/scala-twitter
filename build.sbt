name := "scala-twitter"

version := "1.0-SNAPSHOT"

resolvers ++= Seq(
  "repo.novus rels" at "http://repo.novus.com/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.mongodb" %% "casbah" % "2.7.3",
  "com.novus" %% "salat" % "1.9.9",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"
)

unmanagedBase := baseDirectory.value / "../../jar"

