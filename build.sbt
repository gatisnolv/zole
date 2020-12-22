name := "zole"

version := "0.1"

scalaVersion := "2.13.3"

val catsVersion = "2.2.0"
val catsEffectVersion = "2.2.0"
val http4sVersion = "0.21.13"
val log4CatsVersion = "1.1.1"
val circeVersion = "0.13.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "io.chrisdavenport" %% "log4cats-slf4j" % log4CatsVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "org.scalactic" %% "scalactic" % "3.2.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % Test
)
