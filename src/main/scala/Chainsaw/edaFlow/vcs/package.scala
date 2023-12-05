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

  /** the Optimization of VcsFlow
    * @param enableCoverageType
    *   specify the coverage type will be collected
    * @param parallelNumber
    *   specify the parallel compute core number of VcsFlow
    * @param incrementCompile
    *   enable increment Compile function
    * @param enableMemHierarchy
    *   enhance mem function in VcsFlow (can access mem instance define by multi-mention array)
    * @param noTimingCheck
    *   chose whether use timing check in vcsFlow
    */
  case class VcsCompileOption(
      enableCoverageType: Seq[CoverageType],
      parallelNumber: Int,
      incrementCompile: Boolean,
      enableMemHierarchy: Boolean,
      noTimingCheck: Boolean
  ) extends EdaOptimizeOption

}
