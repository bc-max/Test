ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "LearnScala"
  )

// https://mvnrepository.com/artifact/org.typelevel/cats-core
libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"

