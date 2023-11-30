package Chainsaw.permutation

import spinal.core._
import org.jgrapht.alg.color.SmallestDegreeLastColoring
import org.jgrapht.graph._

import scala.collection.JavaConverters._

/** generic model for Benes network
  * @see
  *   [[BenesNetworkTest]] for test
  */
object Benes {

  /** inner class providing methods accessing control signal
    */
  case class BenesControl[TControl](value: Seq[Seq[TControl]]) {

    val stageCount          = value.length
    val switchCountPerStage = value.head.length
    val dataCount           = switchCountPerStage * 2
    val switchCount         = stageCount * switchCountPerStage

    def getHead: TControl = value.head.head

    def getFirstStage: Seq[TControl] = value.head

    def getLastStage: Seq[TControl] = value.last

    def getMidStages: Seq[Seq[TControl]] = value.drop(1).dropRight(1)

    def getSubUp = BenesControl(getMidStages.map(_.take(switchCountPerStage / 2)))

    def getSubBottom = BenesControl(getMidStages.map(_.takeRight(switchCountPerStage / 2)))

  }

  /** generate stage-by-stage control signals for a specific permutation
    */
  def permutation2Control(permutation: Permutation): BenesControl[Boolean] = {

    val permuted = permutation.permuted
    require(isPow2(permutation.size))
    val n = permuted.size

    if (n == 2) {                                // base solution
      BenesControl(Seq(Seq(permuted.head == 1))) // for 0,1 -> false, else(1,0) true
    } else {

      /** -------- solve current stage problem as by coloring a graph
        * --------
        */
      val colorGraph = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
      permuted.indices.foreach(colorGraph.addVertex) // vertices
      // add constraints by adding edges(connected vertices shouldn't have the same color)
      (0 until n / 2).foreach(i => colorGraph.addEdge(i, i + n / 2))                     // input side constraint
      (0 until n / 2).foreach(i => colorGraph.addEdge(permuted(i), permuted(i + n / 2))) // output side constraint

      // TODO: find best algo for coloring in this problem
      val color: Seq[(Int, Integer)] =
        new SmallestDegreeLastColoring(
          colorGraph
        ).getColoring.getColors.asScala.toSeq // solve the problem, get pairs of vertex->color
      require(
        color.forall(_._2 < 2),
        s"there are ${color.map(_._2).max + 1} > 2 colors in solution"
      ) // 2-color requirement

      /** -------- solution extraction
        * --------
        */
      val up                         = color.filter(_._2 == 0).map(_._1) // vertices colored 0
      val bottom                     = color.filter(_._2 == 1).map(_._1) // vertices colored 1
      val solutionPre: Seq[Boolean]  = up.sortBy(_ % (n / 2)).map(_ >= (n / 2))
      val solutionPost: Seq[Boolean] = up.map(permuted.indexOf(_)).sortBy(_ % (n / 2)).map(_ >= (n / 2))

      /** -------- sub-problem construction
        * --------
        */
      val problem0 = up.sortBy(permuted.indexOf(_) % (n / 2)).map(_ % (n / 2))
      val problem1 = bottom.sortBy(permuted.indexOf(_) % (n / 2)).map(_ % (n / 2))
      val solutionMid: Seq[Seq[Boolean]] =
        permutation2Control(Permutation(problem0)).value         // upper sub-network
          .zip(permutation2Control(Permutation(problem1)).value) // low sub-network
          .map { case (s0, s1) => s0 ++ s1 }

      BenesControl(solutionPre +: solutionMid :+ solutionPost)
    }
  }

  /** do permutation by a BenesNetwork
    *
    * @param dataIn
    *   input data
    * @param controlIn
    *   control signals corresponding to a specific permutation
    * @param butterfly
    *   a function that defines the behavior of a butterfly operator(switch)
    * @tparam TData
    *   input data type, could be software|hardware
    * @tparam TControl
    *   control signal type, could be software|hardware
    * @return
    *   permuted data
    */
  def doBenes[TData, TControl](
      dataIn: Seq[TData],
      controlIn: BenesControl[TControl],
      butterfly: (TData, TData, TControl) => Seq[TData]
  ): Seq[TData] = {

    def getUp[T](dataIn: Seq[T]): Seq[T] = dataIn.take(dataIn.size / 2)

    def getBottom[T](dataIn: Seq[T]): Seq[T] = dataIn.takeRight(dataIn.size / 2)

    def doStage(dataIn: Seq[TData], stageControl: Seq[TControl]): Seq[TData] = {
      val switched = getUp(dataIn)
        .zip(getBottom(dataIn))
        .zip(stageControl)
        .map { case ((a, b), switch) => butterfly(a, b, switch) } // switches
      switched.map(_.head) ++ switched.map(_.last)                // connections
    }

    val n = dataIn.size
    if (n == 2) butterfly(dataIn(0), dataIn(1), controlIn.getHead)
    else {
      // decompose control
      val (pre, post)                      = (controlIn.getFirstStage, controlIn.getLastStage)
      val (subNetworkUp, subNetworkBottom) = (controlIn.getSubUp, controlIn.getSubBottom)
      // build network recursively
      val afterPreOrdered = doStage(dataIn, pre) // pre-network
      val afterSub = doBenes(getUp(afterPreOrdered), subNetworkUp, butterfly) ++ doBenes(
        getBottom(afterPreOrdered),
        subNetworkBottom,
        butterfly
      )                       // recursive build
      doStage(afterSub, post) // post-network
    }
  }

  // simplified user interface
  private def butterfly(a: Int, b: Int, switch: Boolean): Seq[Int] =
    if (switch) Seq(b, a) else Seq(a, b) // do switch when switch is true

  def doBenes(dataIn: Seq[Int], permutation: Permutation): Seq[Int] =
    doBenes(dataIn, permutation2Control(permutation), butterfly)

}
