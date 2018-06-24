Installation steps:

1. Make sure you have [sbt](https://www.scala-sbt.org) (the default build tool for Scala) installed on your machine.

2. (Optional) To better read and modify the source code, a Scala IDE like [Intellij IDEA](https://www.jetbrains.com/help/idea/discover-intellij-idea-for-scala.html) is strongly recommended.

3. In the project root, open a terminal and type `sbt run`. After sbt have downloaded all project dependencies, it will print out all the detected `main` functions and ask you which `main` to run. To run the quickSort example described in the [tutorial](https://github.com/MrVPlusOne/Singularity#tutorial), choose `singularity.examples.QuickSort`. Results are saved into `results-running` and `results`. See also [Tutorial/Saving and Reading Patterns](https://github.com/MrVPlusOne/Singularity#saving-and-reading-patterns)

4. To run the set of Textbook algorithm examples described in the Singularity Paper, in the project root, type
   ```
   sbt assembly
   ```
   This will package everything into `src/benchmarks/target/scala-2.12/singularity-benchmarks-assembly-0.6.jar`, then type
   ```
   java -cp src/benchmarks/target/scala-2.12/singularity-benchmarks-assembly-0.6.jar benchmarks.TextbookExamples
   ```
   to start the 17 textbook algorithm examples described in section 7.1 of the Singularity paper. The additional packaging step is required in order to spawn multiple processes. By default, 8 subprocesses will run in parallel. You can change this number by setting `processNum` in `TextbookExamples.main()`. As described above, all results will be written into `results-running` and `results`.