name := "solr-query-parser"

organization := "org.jtc"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
	"org.parboiled" %% "parboiled" % "2.0.1",
	"org.scalatest" %% "scalatest" % "2.2.2" % "test"
)
