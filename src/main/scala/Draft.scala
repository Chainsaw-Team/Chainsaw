import Chainsaw._

import java.lang.Float.floatToIntBits

object Draft extends App {

  def countLeadingChars(string: String, char: Char): Int =
    string.takeWhile(_ == char).length



  println(countLeadingChars("xxxy", 'x'))

}
