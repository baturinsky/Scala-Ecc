package ecc
import scala.util.Random

class Key(val pub: CurvePoint, sec: BigInt = null, val random: Random = new Random) {
  lazy val curve = pub.curve
  lazy val n = curve.n

  def randomn(r: Random): BigInt = {
    return BigInt.apply(curve.q.bitLength + 1, r) % n
  }

  def jac(P: CurvePoint) = new Jacobian(P)

  def sign(hash: BigInt, fast: Boolean = true, sInverted: Boolean = true): (BigInt, BigInt) = {
    assert(sec != null, "private key not set")
    val k = BigInt.apply(n.bitLength, random) % n    
    
    val r = if (fast)
      (jac(curve.P) * k).affine.x % n
    else
      (curve.P * k).x % n
          
    val s = if (sInverted)
      (k * (hash + sec * r).modInverse(n)) % n
    else
      k.modInverse(n) * (hash + sec * r) % n
    return (r, s)
  }

  def verify(hash: BigInt, signature: (BigInt, BigInt), fast: Boolean = true, sinv: Boolean = true): Boolean = {
    val (r, s) = signature
    val w = if (sinv) s else s.modInverse(n)
    val u1 = hash * w % n
    val u2 = r * w % n
    val X = if (fast)
      (jac(curve.P) * u1 + jac(pub) * u2).affine
    else
      curve.P * u1 + pub * u2
    val v = X.x % n
    v == r
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

  def pub(G: BigInt, curve:Curve): Key = 
    new Key(curve.uncompress(G))
  
  def apply(pub:CurvePoint) =
    new Key(pub)

}