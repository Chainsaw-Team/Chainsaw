package Chainsaw.coding

import breeze.numerics.log2

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** probability distribution of symbols
  */
case class Distribution(probabilities: Seq[Double]) {
  assert(probabilities.sum == 1, "distribution should sum to 1")
  def length = probabilities.length

  def map = (0 until length).zip(probabilities).toMap

  def entropy = -probabilities.map(p => p * log2(p)).sum
}
case class Huffman(distribution: Distribution) {

  val symbolCount = distribution.length
  val codeBook    = mutable.Map[Int, String]()
  (0 until distribution.length).foreach(i => codeBook += ((i, "")))
  def updateSymbol(symbols: Seq[Int], code: Char) =
    symbols.foreach(symbol => codeBook.update(symbol, code + codeBook(symbol)))

  val candidates: mutable.Map[Seq[Int], Double] =
    mutable.Map((0 until symbolCount).map(Seq(_)).zip(distribution.probabilities): _*)

  while (candidates.size > 1) {
    val Seq((s0, p0), (s1, p1)) = candidates.toSeq.sortBy(_._2).take(2)
    updateSymbol(s0, '1')
    updateSymbol(s1, '0')
    candidates.remove(s0)
    candidates.remove(s1)
    candidates += ((s0 ++ s1, p0 + p1))
  }

  println(s"code book = ${codeBook.mkString(" ")}")
  codeBook.values.foreach { word =>
    assert(codeBook.values.filter(_ != word).forall(!_.startsWith(word)), s"not word is ")
  }

  def encode(symbols: Seq[Int]): String = {
    symbols.map(codeBook).mkString
  }

  def decode(bits: String): Seq[Int] = {
    // TODO: solve this by a tree
    val codeSeq    = codeBook.toSeq.sortBy(_._1).map(_._2)
    val candidates = ArrayBuffer[Char](bits: _*)
    val ret        = ArrayBuffer[Int]()

    while (candidates.nonEmpty) {
      val current = ArrayBuffer[Char]()
      while (!codeSeq.contains(current.mkString)) {
        current += candidates.remove(0)
      }
      ret += codeSeq.indexOf(current.mkString)
    }
    ret
  }
}

object Huffman extends App {
  val coutns       = Seq.fill(26)(Random.nextInt(100))
  val distribution = Distribution(coutns.map(_.toDouble / coutns.sum))
  val huffman      = Huffman(distribution)
  val symbols      = Seq.fill(100)(Random.nextInt(26))
  val coded        = huffman.encode(symbols)
  val decoded      = huffman.decode(coded)
  assert(symbols.equals(decoded), s"\nsymbols = ${symbols.mkString(" ")}\ndecoded = ${decoded.mkString(" ")}")
}
