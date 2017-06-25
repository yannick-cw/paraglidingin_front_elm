import sbt._

object Dependencies {

  val akkaHttpVersion = "10.0.5"
  val Seq(akkaHttp, akkaTestkit) = Seq(
    "com.typesafe.akka" %% "akka-http",
    "com.typesafe.akka" %% "akka-http-testkit"
  ).map(_ % akkaHttpVersion)

  val circeVersion = "0.7.1"
  val circe = Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  )

  val serverDependencies = Seq(
    akkaHttp,
    "org.typelevel" % "cats-core_2.12" % "0.9.0",
    akkaTestkit % Test
  ) ++ circe
}
