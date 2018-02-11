package benchmarks

import benchmarks.AllTogether.{ConfigGen, TaskGen}
import patsyn.{IS}

object STACExamples {
  import patsyn.FuzzingTaskProvider


  def allTasks(ioId: Int, workDir: String): IS[FuzzingTaskProvider] = IS(
    FuzzingTaskProvider.bloggerExample(ioId),
    FuzzingTaskProvider.graphAnalyzerExample(workDir),
    FuzzingTaskProvider.airplan1Example(workDir),
    FuzzingTaskProvider.airplan2Example(workDir),
    FuzzingTaskProvider.imageExample(10, 10, workDir),
    FuzzingTaskProvider.linearAlgebraExample(10, workDir)
  )
}
