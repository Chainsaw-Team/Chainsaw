package Chainsaw.permutation

import breeze.linalg._

/** generic model for permutation
  * @param permuted
  *   a sequence of integers in [0, N)
  * @example
  *   a permutation defined by [0,2,1,3] permutes [a,b,c,d] to [a,c,b,d]
  */
case class Permutation(permuted: Seq[Int]) {

  val size = permuted.size
  require(permuted.sorted.equals(permuted.indices))

  /** do permutation
    * @return
    *   permuted data
    */
  def permute[T](dataIn: Seq[T]): Seq[T] = permuted.map(dataIn.apply)

  /** concatenation of permutations
    */
  def concat(that: Permutation) = {
    require(this.size == that.size)
    Permutation(that.permute(permuted))
  }

  def getPermutationMatrix = {
    val content = Array.tabulate(size, size)((i, j) => if (permuted(i) == j) 1 else 0)
    new DenseMatrix(size, size, content.flatten)
  }

  /** @see
    *   ''Automatic Generation of Streaming Datapaths for Arbitrary Fixed Permutations, Peter A. Milder, James C. Hoe,
    *   and Markus P¨uschel'', "mapping \Pi_w"
    */
  def getMappingMatrix(streamWidth: Int): DenseMatrix[Int] = {
    val mappintMatrix =
      Array.tabulate(streamWidth, streamWidth) { (i, j) =>
        permuted.zipWithIndex.count { case (out, in) =>
          (out % streamWidth == j) && (in % streamWidth == i)
        }
      }
    new DenseMatrix(streamWidth, streamWidth, mappintMatrix.flatten)
  }
}

object Permutation {

  /** generate a random permutation of specific size
    */
  def random(size: Int): Permutation = Permutation(scala.util.Random.shuffle((0 until size).toList))
}
