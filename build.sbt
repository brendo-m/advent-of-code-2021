ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2021",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "requests" % "0.6.9",
      "com.lihaoyi" %% "os-lib" % "0.7.8"
    )
  )
