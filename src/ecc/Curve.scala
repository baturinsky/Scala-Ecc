package ecc

class Curve(val q: BigInt, val n: BigInt, val a: BigInt, val b: BigInt, x: BigInt, y: BigInt) {
  val P: CurvePoint = new CurvePoint(this, x, y)
  val bits = q.bitLength
  lazy val aIsQMinus3 = (a == q - 3)

  def point(x: BigInt, y: BigInt) = new CurvePoint(this, x, y)
  def point(x: BigIntMod, y: BigIntMod) = new CurvePoint(this, x.n, y.n)
  def point(xy: (BigInt, BigInt)) = new CurvePoint(this, xy._1, xy._2)

  def uncompress(G: BigInt): CurvePoint = {
    val (x, y) = Curve.uncompress(G, q, a, b)
    point(x, y)
  }

  override def toString =
    "q:  " + q.toString(16) + "\n" +
      "n:  " + n.toString(16) + "\n" +
      "a:  " + a.toString(16) + "\n" +
      "b:  " + b.toString(16) + "\n" +
      "x:  " + P.x.toString + "\n" +
      "y:  " + P.y.toString + "\n"
}

object Curve {

  def uncompress(G: BigInt, q: BigInt, a: BigInt, b: BigInt): (BigInt, BigInt) = {
    val len: Int = G.bitLength >> 3 << 3
    val b1 = G.testBit(len + 1)
    val b2 = G.testBit(len)
    val isCompressed = b1 || b2
    if (isCompressed) {
      val cut: BigInt = BigInt(1) << len
      val x = new BigIntMod(G % cut, q)
      val root = (x.pow(3) + x * a + b).root
      (x.n, if (b2) root._1.n else root._2.n)
    } else {
      val cut: BigInt = BigInt(1) << (len / 2)
      ((G >> (len / 2)) % cut, G % cut)
    }
  }

  private implicit def hexToBigint(s: String): BigInt =
    BigInt(s.replaceAll("[^-0-9a-fA-F]", ""), 16)

  def apply(q: String, n: String, _a: String = "-3", b: String, G: String) = {
    val a = (BigInt(_a) + q) % q
    val (x, y) = uncompress(G, q, a, b)
    new Curve(q, n, a, b, x, y)
  }

}
