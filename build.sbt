name := """rsi_ws"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "com.wordnik" %% "swagger-play2" % "1.3.10",
  "org.scalikejdbc" %% "scalikejdbc"                       % "2.1.4",
  "org.scalikejdbc" %% "scalikejdbc-play-plugin"           % "2.3.3",
  "org.scalikejdbc" %% "scalikejdbc-play-fixture-plugin"   % "2.3.3"
)

