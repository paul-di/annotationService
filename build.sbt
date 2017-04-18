organization := "com.example"

name := "annotation"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.1"

val unusedWarnings = (
  "-Ywarn-unused" ::
  "-Ywarn-unused-import" ::
  Nil
)

scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
  case Some((2, v)) if v >= 11 => unusedWarnings
}.toList.flatten

Seq(Compile, Test).flatMap(c =>
  scalacOptions in (c, console) --= unusedWarnings
)

scalacOptions ++= "-deprecation" :: "unchecked" :: "-feature" :: Nil

val unfilteredVersion = "0.9.1"

libraryDependencies ++= Seq(
  "ws.unfiltered" %% "unfiltered-directives" % unfilteredVersion,
  "ws.unfiltered" %% "unfiltered-filter" % unfilteredVersion,
  "ws.unfiltered" %% "unfiltered-jetty" % unfilteredVersion,
  "ws.unfiltered" %% "unfiltered-specs2" % unfilteredVersion % "test"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.apache.opennlp" % "opennlp-tools" % "1.7.2"
