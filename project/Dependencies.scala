import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val akkaHttp = "com.typesafe.akka" %% "akka-http" % "10.1.3"
  lazy val akkaHttpTest = "com.typesafe.akka" %% "akka-http-testkit" % "10.1.3"
  lazy val swing = "org.scala-lang.modules" %% "scala-swing" % "2.0.3"
}
