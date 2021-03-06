import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "ashashev",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Evolution",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      scalaCheck % Test,
      akkaHttp,
      akkaHttpTest % Test,
      swing
    ),
    scalacOptions ++= ScalacFlags.options,
    scalacOptions in Test ++= ScalacFlags.options
  )
