import Dependencies._

lazy val root = project
  .in(file("."))
  .settings(
    name := "lang4",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.2.0",
    libraryDependencies += scalaTest % Test
  )
  .dependsOn(libparse)

lazy val libparse = project
  .in(file("libparse"))
  .settings(
    name := "libparse",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.2.0",
    libraryDependencies += scalaTest % Test
  )
