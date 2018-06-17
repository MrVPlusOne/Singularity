# Singularity

Pattern-based Fuzzing framework for determing the worst-case algorithmic complexity.


## Overview

*The closer your program gets to a singularity, the slower it runs.*

<p align="right"><strong> -- general theory of relativity </strong></p>

Singularity is an automatic fuzzing tool for generating inputs (called patterns) that demonstrates the maximal resource usage behavior of a given program.

The core idea behind Singulairty is based on the observations that the worst-case behavior of a given progarm is usually triggered by inputs with some specific structual patterns. For example, to trigger the maximal running time of an insertion sort algorithm, the input array must be reversely sorted. Similarly, inserting a series of elements with the right pattern into [a hash-based data structure](https://github.com/google/guava/issues/3015) can cause hash collisions and greatly decrease its performance.

In order to efficiently search for such inputs, Singularity emploies a Domain Specific Language (DSL) to compactly represents different input patterns and uses an optimization technique called [Genetic Programming](https://en.wikipedia.org/wiki/Genetic_programming) to synthesize input patterns generating maximial resource usage inputs. This feedback-guided workflow is shown as follows:


<img src="doc/images/SingularityLoop.png" width="600">


## Usage

The recommended way is to include Singularity as a library into your Scala/Java project. Since Singularity is a blackbox fuzzing technique, it can be used for fuzzing target programs written in any language, but it would require the user to provide gluing code to translate the data structures outputed by Singularity into corresponding counterparts acceptable by the program of interest.

To include Singularity into your Scala project, add the following line into your build.sbt file.

```scala
libraryDependencies += "TODO"
```

## QuickSort example


- [A quick tutorial](Tutorial.md)