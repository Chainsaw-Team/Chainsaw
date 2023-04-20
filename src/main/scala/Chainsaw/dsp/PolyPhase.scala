package Chainsaw.dsp

case class Var(name: String)
case class Term(coeff: Int, vars: Seq[Var], orders: Seq[Int], delay: Int) {

  assert(vars.length == orders.length)
  private def simplified = { // remove the zero order terms
    val nonZeros = vars.zip(orders).filter(_._2 != 0)
    Term(coeff, nonZeros.map(_._1), nonZeros.map(_._2), delay)
  }

  // FIXME: to identify the "like" terms, may failed as hash function may have collision
  def id = (this.vars, this.orders, delay).hashCode()

  def getOrder(v: Var) = {
    val index = vars.indexOf(v)
    if (index == -1) 0 else orders(index)
  }

  def order = orders.sum
  def +(that: Term) = {
    assert(this.id == that.id)
    Term(this.coeff + that.coeff, this.vars, this.orders, this.delay)
  }.simplified

  def *(that: Term) = {
    val newCoeff  = this.coeff * that.coeff
    val newVars   = (this.vars ++ that.vars).distinct
    val newOrders = newVars.map(v => this.getOrder(v) + that.getOrder(v))
    val newDelay  = this.delay + that.delay
    Term(newCoeff, newVars, newOrders, newDelay)
  }.simplified

  def pow(n: Int) = Term(coeff, vars, orders.map(_ * n), delay * n)

  def slow(n: Int) = Term(coeff, vars, orders, delay * n)

  def withName(name: String) = {
    assert(vars.length == 1)
    Term(coeff, Seq(Var(name)), orders, delay)
  }

  def getDelayed(cycle: Int) = Term(coeff, vars, orders, delay + cycle)

  override def toString =
    (if (coeff == 0) "" else coeff.toString) + vars
      .zip(orders)
      .map { case (v, o) => s"(${v.name}^$o)" }
      .mkString("") + s"z^-$delay"
}

object Term {
  def apply(v: Var) = new Term(1, Seq(v), Seq(1), 0)
}

case class Polynomial(terms: Seq[Term]) {

  private def simplified = {
    val grouped       = terms.groupBy(_.id)
    val combinedTerms = grouped.map { case (_, terms) => terms.reduce(_ + _) }
    Polynomial(combinedTerms.toSeq)
  }
  def +(that: Polynomial) = Polynomial(this.terms ++ that.terms).simplified
  def *(that: Polynomial) = Polynomial(
    this.terms.flatMap(t1 => that.terms.map(t2 => t1 * t2))
  ).simplified

  val vars = terms.flatMap(_.vars).distinct

  def getPolyPhased(phaseCount: Int) = {
    assert(terms.forall(_.vars.length == 1))
    val newTerms =
      terms.flatMap(t =>
        (0 until phaseCount).map(i => t.slow(phaseCount).withName(s"${t.vars.head.name}_$i").getDelayed(i))
      )
    Polynomial(newTerms)
  }

  def getUpSampled(factor: Int) = {
    assert(terms.forall(_.vars.length == 1))
    val newTerms = terms.zipWithIndex.map { case (term, i) =>
      term.slow(factor).withName(s"${term.vars.head.name}_$i")
    }
    Polynomial(newTerms)
  }

  override def toString = terms.mkString(" + ")
}

object Polynomial {

  def apply(term: Term) = new Polynomial(Seq(term))
  def apply(v: Var)     = new Polynomial(Seq(Term(v)))
  def main(args: Array[String]): Unit = { // example

//    val xs = Polynomial(Var("x")).getPolyPhased(3 )
//    val hs = Polynomial(Var("h")).getPolyPhased(3)
//    val product = xs * hs
//    val ys      = product.terms.groupBy(_.delay % 3).map { case (_, terms) => Polynomial(terms) }.toSeq

    val xs      = Polynomial(Var("x")).getPolyPhased(3).getUpSampled(3)
    val hs      = Polynomial(Var("h")).getPolyPhased(3)
    val product = xs * hs
    val ys      = product.terms.groupBy(_.delay % 3).map { case (_, terms) => Polynomial(terms) }.toSeq
    println(ys.mkString("\n"))

  }
}
