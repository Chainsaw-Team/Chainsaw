package Chainsaw.edaFlow

import java.io.File

package object vcs {
  sealed trait CoverageType

  object LineCoverage extends CoverageType
  object CondCoverage extends CoverageType
  object FsmCoverage extends CoverageType
  object TglCoverage extends CoverageType
  object PathCoverage extends CoverageType
  object BranchCoverage extends CoverageType
  object AssertCoverage extends CoverageType
  object FullCoverage extends CoverageType

  case class VcsCompileOption(
      enableCoverageType: Seq[CoverageType],
      parallelNumber: Int,
      incrementCompile: Boolean,
      enableMemHierarchy: Boolean,
      noTimingCheck: Boolean
  ) extends EdaOptimizeOption

}
