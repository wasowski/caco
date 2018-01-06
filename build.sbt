lazy val root = (project in file(".")).settings (

  organization := "caco",
  name := "caco",
  version := "0.00",
  // stick to Scala version supported by Scala IDE 
  // scalaVersion := "2.11.11",
  retrieveManaged := true,

  scalacOptions += "-deprecation",
  scalacOptions += "-feature",
  // scalacOptions += "-language:implicitConversions",

  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)
