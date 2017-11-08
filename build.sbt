name := "PatternSyn"

version := "0.1"

scalaVersion := "2.12.3"

//libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11"
libraryDependencies ++= Seq(
  "org.jfree" % "jfreechart" % "1.0.14",
  "com.typesafe.akka" %% "akka-actor" % "2.5.6",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.6" % Test,
  "org.apache.commons" % "commons-lang3" % "3.1"
)