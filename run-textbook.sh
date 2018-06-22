#!/usr/bin/env bash
sbt assembly
java -cp src/benchmarks/target/scala-2.12/singularity-benchmarks-assembly-0.6.jar benchmarks.TextbookExamples