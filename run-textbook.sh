#!/usr/bin/env bash
sbt assembly
java -cp target/scala-2.12/singularity-assembly-0.6.jar singularity.benchmarks.TextbookExamples