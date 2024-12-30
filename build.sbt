ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

lazy val root = (project in file("."))
  .settings(
    name := "double"
  )

libraryDependencies += "org.scalameta" %% "munit"  % "1.0.3" % Test
libraryDependencies += "com.lihaoyi"   %% "pprint" % "0.9.0" % Test
