name := "StarRealmsCG"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)