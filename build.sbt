name := "udemy-akka-essentials"

version := "0.1"

scalaVersion := "2.13.5"

val akkaVersion = "2.6.14"
val scalaTestVersion = "3.2.7"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion,
)
