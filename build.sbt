ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-streams" % "2.0.4",
      "com.lihaoyi" %% "fastparse" % "2.3.3"
    )
  )
