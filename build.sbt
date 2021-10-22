name := "tcd"

version := "0.1"



lazy val commonSettings = Seq(
  organization := "lil_insulin",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "3.0.2",
  Compile / scalacOptions ++= Seq(
      "-Ykind-projector",
      "-rewrite",
      "-indent",
      "-language:implicitConversions",
  ),
  scalacOptions := Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Ytasty-reader",
    "-language:implicitConversions"
//    "-Yexplicit-nulls"
  ),
  resolvers ++= Seq(
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  ),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val root = (project in file(".")).aggregate(tc)


lazy val tc = (project in file("tc")).settings(
  commonSettings
).dependsOn(interface)