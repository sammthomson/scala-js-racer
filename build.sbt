enablePlugins(ScalaJSPlugin)

name := "scalajs-racer"

organization := "org.samthomson"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.bintrayRepo("sammthomson", "maven")
)

libraryDependencies ++= Seq(
  "org.samthomson" %%% "scalajs-pointer-events-polyfill" % "0.1"
)
