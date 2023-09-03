import Chainsaw._

import java.lang.Float.floatToIntBits


object Draft extends App {

  val a = 3.14f
  print(BigInt(floatToIntBits(a)).toString(2).padToLeft(32, '0'))
}
