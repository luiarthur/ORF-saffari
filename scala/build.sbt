name := "orf"
assemblyJarName in assembly := "orf.jar"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

scalaVersion := "2.10.6"
