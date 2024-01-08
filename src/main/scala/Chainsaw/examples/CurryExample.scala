package Chainsaw.examples

object CurryExample {

  def add(a: Int, b: Int) = a + b

  def add1 = add(_, 1)

  def main(args: Array[String]): Unit = {
    println(add1(1))
  }

}

object CurryApp extends App {

  println(CurryExample.add1(1))

}
