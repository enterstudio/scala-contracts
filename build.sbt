name := "scala-contracts"

version := "0.0.1"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
	"joda-time" % "joda-time" % "2.1",
	"org.joda" % "joda-convert" % "1.2"
)
