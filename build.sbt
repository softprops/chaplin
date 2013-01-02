organization := "me.lessis"

name := "chaplin"

version := "0.1.0-SNAPSHOT"

description := "Mustache templating for Scala"

scalacOptions += "-deprecation"

libraryDependencies ++= Seq("org.apache.commons" % "commons-lang3" % "3.1",
                            "org.scalatest" %% "scalatest" % "1.8" % "test",
                          "net.databinder.dispatch" %% "dispatch-lift-json" % "0.9.4" % "test")

testOptions in Test += Tests.Argument("-oD")
