package Chainsaw.project.zprize

object ZPrizeMSM {

  /** -------- parameters
    * --------
    */
  val z = BigInt("8508c00000000001", 16)
  // (z-1)^2(z^4 - z^2 + 1)/3+z
  val baseModulus =
    BigInt("01ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001", 16)
  val baseRoot =
    BigInt("00f3c1414ef58c54f95564f4cbc1b61fee086c1fe367c33776da78169a7f3950f1bd15c3898dd1af1c104955744e6e0f", 16)

  val poseidonModulus = BigInt("73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001", 16)

  // (z^4-z^2+1)
  val scalarModulus = BigInt("12ab655e9a2ca55660b44d1e5c37b00159aa76fed00000010a11800000000001", 16)
  val scalarRoot    = BigInt("0d1ba211c5cc349cd7aacc7c597248269a14cda3ec99772b3c3d3ca739381fb2", 16)

  // for Montgomery algo
  val R        = BigInt(1) << baseModulus.bitLength
  val RInverse = R.modInverse(baseModulus)
  val NPrime   = ((R * RInverse - 1) / baseModulus).mod(baseModulus)

  // for Barrett algo
  val MPrime = (BigInt(1) << (2 * baseModulus.bitLength)) / baseModulus

  //  // the elliptic curve that we use
  //  val a = asBigInteger(0)
  //  val b = asBigInteger(1)
  //  implicit val ec: EcGroup = EcGroup(baseModulus, a, b) // y^2 = x^3 + 1
  //
  //  // points for simulation
  //  val src = Source.fromFile("/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/BLS12377POINTS")
  //  val lines = src.getLines().toSeq
  //  //  src.close()
  //  val points: Seq[EcPointAffine] = lines.map { line =>
  //    val Seq(xline, yline) = line.split(" ").toSeq
  //    val Seq(x, y) = Seq(xline, yline).map(str => BigInt(str.drop(2), 16))
  //    EcPointAffine(x, y)
  //  }
  //
  //  val N = 1 << 26
  //  val W = 253
  //  val w = 4
  //
  //  def msm(k: Seq[IntZ], p: Seq[EcPointAffine]) = {
  //    val add = (a: EcPointAffine, b: EcPointAffine) => a + b
  //    val dbl = (a: EcPointAffine) => a + a
  //    PippengerOld(N, W, w, add, dbl, EcZeroAffine).doPippenger(k.map(_.toBigInt), p)
  //  }
  //
  //  def main(args: Array[String]): Unit = {
  //    assert(baseModulus == (z - 1).pow(2) * (z.pow(4) - z.pow(2) + 1) / 3 + z)
  //    assert(scalarModulus == z.pow(4) - z.pow(2) + 1)
  //  }
}
