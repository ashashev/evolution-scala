import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "ashashev",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Evolution",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      akkaHttp,
      akkaHttpTest % Test,
      swing
    )
  )
