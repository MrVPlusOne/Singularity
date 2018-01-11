name := "PatternSyn"

version := "0.1"

scalaVersion := "2.12.3"

//libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11"
libraryDependencies ++= Seq(
  "org.jfree" % "jfreechart" % "1.0.14",
  "com.typesafe.akka" %% "akka-actor" % "2.5.6",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.6" % Test,
  "org.apache.commons" % "commons-lang3" % "3.1",
  "com.github.scopt" %% "scopt" % "3.7.0",
  "org.scalaj" %% "scalaj-http" % "2.3.0"
)

mainClass in assembly := Some("cli.BenchmarkDriver")
test in assembly := {}
assemblyJarName in assembly := "PatternSynthBenchmarkDriver.jar"

//Ammonite
libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.3" % "test" cross CrossVersion.full

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue