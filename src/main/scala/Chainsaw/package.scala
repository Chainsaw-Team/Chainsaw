
package object Chainsaw {



  val dot = "■"

  implicit class StringUtil(s: String) {

    // complement version of method padTo(padToRight)
    def padToLeft(len: Int, elem: Char) = s.reverse.padTo(len, elem).reverse

    def repeat(times: Int) = Seq.fill(times)(s).reduce(_ + _)
  }

}
