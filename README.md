# Singularity

#### Pattern-based blackbox fuzzing framework for determing worst-case algorithmic complexities.


## Overview

**The closer your program gets to a singularity, the slower it runs.**

<p align="right"><strong> -- general theory of relativity </strong></p>

Singularity is an automatic fuzzing tool for generating inputs (called patterns) that demonstrates the maximal resource usage behavior of a given program.

The core idea behind Singulairty is based on the observation that the worst-case behavior of a given progarm is normally triggered by inputs with some specific structual patterns. For example, to trigger the maximal running time of an insertion sort algorithm, the input array must be reversely sorted. Similarly, inserting a series of elements with the right pattern into a hash-based data structure can cause hash collisions and greatly decrease its performance (see [this github issue](https://github.com/google/guava/issues/3015)).

In order to efficiently search for such inputs, Singularity emploies a Domain Specific Language (DSL) to compactly represents different input patterns and uses an optimization technique called [Genetic Programming](https://en.wikipedia.org/wiki/Genetic_programming) to synthesize input patterns with maximial resource usage inputs. This feedback-guided workflow is shown as follows:

![SingularityLoop.png](doc/images/SingularityLoop.png)


## Usage

The recommended way is to include Singularity as a library into your Scala/Java project. Since Singularity is a blackbox fuzzing technique, it can be used to fuzz target programs written in any language, but the user is required to provide gluing code to translate the data structures outputed by Singularity into corresponding counterparts acceptable by the target program.

To include Singularity into your Scala project, add the following line into your `build.sbt` file.

```scala
libraryDependencies += "TODO"
```

## QuickSort example


- [A quick tutorial](Tutorial.md)


## Notes

- To obtain deterministic performance measurements, we tend to rely on program instrumentation: [This tool](https://github.com/grievejia/CostInstrument) is used to instrument programs written in Java, and [this tool](https://github.com/grievejia/CostInstrument-llvm) is used to instrument programs written in C/C++. 
