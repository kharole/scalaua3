name := """scalaua3"""
organization := "com.example"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.3"

libraryDependencies += ws
libraryDependencies += filters
libraryDependencies += guice
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.11" withSources() withJavadoc()
libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % "2.5.11" withSources() withJavadoc()
//libraryDependencies += "com.typesafe.akka" %% "akka-persistence-cassandra" % "0.83" withSources() withJavadoc()
libraryDependencies += "org.iq80.leveldb"            % "leveldb"          % "0.7"
libraryDependencies += "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8"

libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
libraryDependencies += "org.awaitility" % "awaitility" % "3.0.0" % Test

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.example.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.example.binders._"
