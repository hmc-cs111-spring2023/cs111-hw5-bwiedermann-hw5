val scala3Version = "3.2.2"

// ThisBuild / name := "Scala machines"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3Version
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-explain"
)
ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
ThisBuild / libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % Test

lazy val dfa = project
lazy val regex = project
lazy val conversion = project.dependsOn(dfa, regex)
