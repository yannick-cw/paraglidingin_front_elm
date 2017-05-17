import Dependencies._
import sbt._

inThisBuild(
  Seq(
    version := "0.1",
    scalaVersion := "2.12.2",
    organization := "io.para"
  ))

lazy val `para-front` = (project in file("."))
  .aggregate(`server`)
  .settings(run := (run in `server` in Compile).evaluated)

lazy val `server` = (project in file("app/jvm"))
  .settings(
    name := "para_elm_front",
    libraryDependencies ++= serverDependencies
  )
