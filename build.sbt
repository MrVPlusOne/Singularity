organization in ThisBuild := "mrvplusone.github.io"
scalaVersion in ThisBuild := "2.12.3"


lazy val core = {
  (project in file("src/core")).settings(
    name := "singularity-core",
    version := "0.6",
    scalaSource in Compile :=  baseDirectory.value / "scala",
    javaSource in Compile :=  baseDirectory.value / "java",
    libraryDependencies ++= {
      Seq(
        "org.jfree" % "jfreechart" % "1.0.14",
        "com.typesafe.akka" %% "akka-actor" % "2.5.6",
        "com.typesafe.akka" %% "akka-testkit" % "2.5.6" % Test,
        "com.github.scopt" %% "scopt" % "3.7.0",
        "com.lihaoyi" %% "ammonite-ops" % "1.0.3"
      )
    }
  )
}

lazy val benchmarks = {
  (project in file("src/benchmarks")).dependsOn(core).
    settings(
    name := "singularity-benchmarks",
    version := "0.6",
    scalaSource in Compile :=  baseDirectory.value / "scala",
    javaSource in Compile :=  baseDirectory.value / "java",
      unmanagedBase := baseDirectory.value / "lib",
    libraryDependencies ++= {
      Seq(
        "com.google.guava" % "guava" % "23.6-jre",
        "org.apache.commons" % "commons-lang3" % "3.1",
        "org.apache.commons" % "commons-collections4" % "4.1",
        "org.apache.commons" % "commons-compress" % "1.15",
        "org.apache.commons" % "commons-math3" % "3.6.1",
        "org.tukaani" % "xz" % "1.6"
      )
    }
  )
}

//mainClass in assembly := Some("cli.BenchmarkDriver")
//test in assembly := {}
//assemblyJarName in assembly := "PatternSynthBenchmarkDriver.jar"



//libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11"
//libraryDependencies ++= Seq(
//  "org.jfree" % "jfreechart" % "1.0.14",
//  "com.typesafe.akka" %% "akka-actor" % "2.5.6",
//  "com.typesafe.akka" %% "akka-testkit" % "2.5.6" % Test,
//  "com.github.scopt" %% "scopt" % "3.7.0",
//  "com.google.guava" % "guava" % "23.6-jre",
//  "org.apache.commons" % "commons-lang3" % "3.1",
//  "org.apache.commons" % "commons-collections4" % "4.1",
//  "org.apache.commons" % "commons-compress" % "1.15",
//  "org.apache.commons" % "commons-math3" % "3.6.1",
//  "org.tukaani" % "xz" % "1.6"
//)
//
//libraryDependencies += "com.lihaoyi" %% "ammonite-ops" % "1.0.3"