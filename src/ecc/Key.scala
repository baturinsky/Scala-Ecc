package ecc
import scala.util.Random

class Key(val pub: CurvePoint, sec: BigInt = null, val random: Random = new Random) {
  lazy val curve = pub.curve
  lazy val n = curve.n

  def jac(P: CurvePoint) = new Jacobian(P)

  def sign(hash: BigInt, fast: Boolean = true, sInverted: Boolean = true): (BigInt, BigInt) = {
    assert(sec != null, "private key not set")

    val k = BigIntMod(BigInt.apply(n.bitLength, random), n)

    val r = BigIntMod(
      if (fast)
        (jac(curve.P) * k).affine.x
      else
        (curve.P * k).x, n)

    val s = if (sInverted)
      k / (r * sec + hash)
    else
      (r * sec + hash) / k
    return (r, s)
  }

  def verify(hash: BigInt, signature: (BigInt, BigInt), fast: Boolean = false, sInverted: Boolean = true): Boolean = {

    val r = BigIntMod(signature._1, n)
    val s = BigIntMod(signature._2, n)
    val w = if (sInverted) s else s.inv
    val u1 = w * hash
    val u2 = w * r

    val X = if (fast)
      (jac(curve.P) * u1 + jac(pub) * u2).affine
    else
      curve.P * u1 + pub * u2

    r == X.x
  }

  override def toString =
    pub.toString() +
      (if (sec == null) "" else "\nsec: " + sec.toString(16))
}

object Key {
  def apply(pub: (BigInt, BigInt), curve: Curve) =
    new Key(curve.point(pub))

  def sec(sec: BigInt, curve: Curve) =
    new Key(curve.P * sec, sec)

  def pub(G: BigInt, curve: Curve): Key =
    new Key(curve.uncompress(G))

  def apply(pub: CurvePoint) =
    new Key(pub)

}