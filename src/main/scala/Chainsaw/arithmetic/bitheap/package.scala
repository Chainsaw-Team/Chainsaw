package Chainsaw.arithmetic

import Chainsaw._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

package object bitheap {

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
      case "Compressor1to1" => Compressor1to1(width, complementHeap)
      case "Compressor3to2" => Compressor3to2(complementHeap)
      case "Compressor6to3" => Compressor6to3(complementHeap)
      case "Compressor3to1" => Compressor3to1(width, 0, complementHeap)
      case "Compressor4to2" => Compressor4to2(width, complementHeap)
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
          s"${solver.solverName}:\n$solution\n"
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
