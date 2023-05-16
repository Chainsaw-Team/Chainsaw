package Chainsaw.arithmetic

import Chainsaw._

import scala.collection.mutable
import scala.math.BigInt

package object bitheap {

  type ST = BigInt

  val candidateCompressors = RowAdders() ++ Gpcs()
  val solverSet = mutable.Set[BitHeapSolver](
    NaiveSolver,
    GreedSolver,
    StrategySeparationSolver,
    StrategySeparationOptimizedSolver,
    TernaryTreeSolver,
    GpcSolver
  )

  def getCompressor(
      compressorName: String,
      width: Int                        = -1,
      complementHeap: Seq[Seq[Boolean]] = null
  ): CompressorGenerator = {
    compressorName.firstBeforeChar('_') match {
      case "Compressor1to1"        => Compressor1to1(width, complementHeap)
      case "Compressor3to2"        => Compressor3to2(complementHeap)
      case "Compressor6to3"        => Compressor6to3(complementHeap)
      case "Compressor3to1"        => Compressor3to1(width, 0, complementHeap)
      case "Compressor4to2"        => Compressor4to2(width, complementHeap)
      case "Compressor15to3"       => Compressor15to3(complementHeap)
      case "Compressor5to3"        => Compressor5to3(complementHeap)
      case "Compressor14to3"       => Compressor14to3(complementHeap)
      case "Compressor23to3"       => Compressor23to3(complementHeap)
      case "Compressor4to2V2"      => Compressor4to2V2(width, complementHeap)
      case "Compressor1415to5"     => Compressor1415to5(complementHeap)
      case "Compressor1406to5"     => Compressor1406to5(complementHeap)
      case "Compressor1325to5"     => Compressor1325to5(complementHeap)
      case "Compressor623to5"      => Compressor623to5(complementHeap)
      case "Compressor606to5"      => Compressor606to5(complementHeap)
      case "Compressor615to5"      => Compressor615to5(complementHeap)
      case "Compressor14051415to9" => Compressor14051415to9(complementHeap)
      case "Compressor13241415to9" => Compressor13241415to9(complementHeap)
      case "Compressor06051415to9" => Compressor06051415to9(complementHeap)
      case "Compressor14050623to9" => Compressor14050623to9(complementHeap)
      case "Compressor13240623to9" => Compressor13240623to9(complementHeap)
      case "Compressor06050623to9" => Compressor06050623to9(complementHeap)
      case "Compressor14050615to9" => Compressor14050615to9(complementHeap)
      case "Compressor13240615to9" => Compressor13240615to9(complementHeap)
      case "Compressor06050615to9" => Compressor06050615to9(complementHeap)
      case "Compressor14141406to9" => Compressor14141406to9(complementHeap)
      case "Compressor06221406to9" => Compressor06221406to9(complementHeap)
      case "Compressor06141406to9" => Compressor06141406to9(complementHeap)
      case "Compressor14141325to9" => Compressor14141325to9(complementHeap)
      case "Compressor06221325to9" => Compressor06221325to9(complementHeap)
      case "Compressor06141325to9" => Compressor06141325to9(complementHeap)
      case "Compressor14140606to9" => Compressor14140606to9(complementHeap)
      case "Compressor06220606to9" => Compressor06220606to9(complementHeap)
      case "Compressor06140606to9" => Compressor06140606to9(complementHeap)
    }
  }

  def searchBestSolver(
      bitHeapGroup: BitHeapGroup[BigInt],
      compressionStrategy: CompressionStrategy = ReFirst,
      detailReport: Boolean                    = false
  ): CompressorFullSolution = {
    val solverSolutions       = solverSet.toSeq.map(solver => (solver, solver.solveAll(bitHeapGroup.copy)))
    var target                = ""
    var result: BitHeapSolver = NaiveSolver

    compressionStrategy match {
      case BrFirst =>
        target = "maximize bit reduction"
        result = solverSolutions
          .map { case (solver, solution) =>
            (solver, solution.scores.getOrElse("br", 0.0))
          }
          .maxBy(_._2)
          ._1
      case ReFirst =>
        target = "maximize reduction efficiency"
        result = solverSolutions
          .map { case (solver, solution) =>
            (solver, solution.scores.getOrElse("re", 0.0))
          }
          .maxBy(_._2)
          ._1
      case HrFirst =>
        target = "maximize heightReduction efficiency"
        result = solverSolutions
          .map { case (solver, solution) =>
            (solver, solution.scores.getOrElse("hr", 0.0))
          }
          .minBy(_._2)
          ._1
      case RrFirst =>
        target = "maximize reduction ratio"
        result = solverSolutions
          .map { case (solver, solution) =>
            (solver, solution.scores.getOrElse("rr", 0.0))
          }
          .maxBy(_._2)
          ._1
      case MinCost =>
        target = "minimize cost"
        result = solverSolutions
          .map { case (solver, solution) =>
            (solver, solution.scores.getOrElse("cost", 0.0))
          }
          .minBy(_._2)
          ._1
    }

    if (detailReport) {
      val header = s"------------BitHeapSolver Search summary------------\n"
      val configInfo =
        s"\n1.Configuration information of Search:\n\n" + s"Candidate Solver: ${solverSet.toSeq
          .map(_.solverName)
          .mkString(",")}\n\n" + s"target: $target\n"
      val searchResult = s"\n2.Search result(The best Solver) is :$result\n"
      val body = s"\n3.Search report are as follows:\n\n" + solverSolutions
        .map { case (solver, solution) =>
          s"${solver.solverName}:\n${solution.tableReport}\n"
        }
        .mkString("\n")
      logger.info(
        s"$header$configInfo$searchResult$body"
      )
    } else {
      logger.info(s"BitHeapSolver Search result is: $result\n")
    }

    solverSolutions
      .find(_._1.solverName == result.solverName)
      .getOrElse(solverSolutions.head)
      ._2
  }
}
