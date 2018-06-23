Installation steps:

1. Make sure you have [sbt](https://www.scala-sbt.org) (the default build tool for Scala) installed on your machine.

2. (Optional) To better read and modify the source code, a Scala IDE like [Intellij IDEA](https://www.jetbrains.com/help/idea/discover-intellij-idea-for-scala.html) is strongly recommended.

3. In the project root, open a terminal and type `sbt run`. After sbt have downloaded all project dependencies, it will print out all the detected `main` functions and ask you which `main` to run. To run the quickSort example described in the [tutorial](https://github.com/MrVPlusOne/Singularity#tutorial), choose `singularity.examples.QuickSort`.