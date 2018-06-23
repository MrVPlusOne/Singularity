package singularity.benchmarks

import singularity._

object STACExamples {
  import singularity.FuzzingTaskProvider


  def allTasks(ioId: Int, workDir: String): IS[FuzzingTaskProvider] = IS(
    ExampleAlgorithms.bloggerExample(ioId),
    ExampleAlgorithms.graphAnalyzerExample(workDir),
    ExampleAlgorithms.airplan1Example(workDir),
    ExampleAlgorithms.airplan2Example(workDir),
    ExampleAlgorithms.imageExample(10, 10, workDir),
    ExampleAlgorithms.linearAlgebraExample(10, workDir)
  )
}
