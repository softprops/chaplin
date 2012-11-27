organization := "me.lessis"

name := "chaplin"

version := "0.1.0-SNAPSHOT"

description := "Mustache templating for Scala"

scalacOptions += "-deprecation"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

testOptions in Test += Tests.Argument("-oD")
