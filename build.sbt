name := "refreshCats"

version := "0.1"

scalaVersion := "2.13.7"

val catsVersion = "2.6.1"

libraryDependencies ++= Seq(

  "org.typelevel" %% "cats-core" % catsVersion
)
