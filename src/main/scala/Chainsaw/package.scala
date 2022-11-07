
import org.slf4j.LoggerFactory
import spinal.core._
import spinal.lib._

import scala.collection.mutable

package object Chainsaw {


  /** --------
   * global run-time environment
   * -------- */
  val logger = LoggerFactory.getLogger("Chainsaw logger") // global logger
  var verbose = 0

  val naiveSet = mutable.Set[String]()
  var atSimTime = true

  val dot = "■"

  /** --------
   * type def
   -------- */
  type Metric = (Any, Any) => Boolean
  type FrameMetric = (Seq[Any], Seq[Any]) => Boolean

  implicit class StringUtil(s: String) {

    // complement version of method padTo(padToRight)
    def padToLeft(len: Int, elem: Char) = s.reverse.padTo(len, elem).reverse

    def repeat(times: Int) = Seq.fill(times)(s).reduce(_ + _)
  }

  // extension of Data
  implicit class DataUtil[T <: Data](data: T) {
    def d(cycle: Int = 1): T = Delay(data, cycle)

  }

  // extension of Bool
  implicit class BoolUtil(data: Bool) {
    // drive a flag which is initially unset
    def validAfter(cycle: Int): Bool = Delay(data, cycle, init = False)
  }

}
