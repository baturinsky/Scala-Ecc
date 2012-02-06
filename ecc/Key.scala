package ecc
import scala.util.Random

object Key{
  def fromPub(pub:(BigInt, BigInt), curve:Curve = ECDSA.defaultCurve) = {
    new Key(curve.point(pub))
  }

  def fromSec(sec:BigInt, curve:Curve = ECDSA.defaultCurve) = {
    new Key(curve.P * sec, sec)
  }
  
  def fromCompPub(comp:BigInt, curve:Curve = ECDSA.defaultCurve) = 
    new Key(ECDSA.unpackPoint(comp, curve))

}

class Key(val pub:CurvePoint, sec:BigInt = null, val random:Random = new Random()) {
  lazy val curve = pub.curve
  lazy val n = curve.n

  def randomn(r:Random) : BigInt = {
    return BigInt.apply(curve.q.bitLength+1, r) % n
  }
  
  def compPub = 
    pub.x + (if (pub.x.testBit(0)) 3 else 2)<<curve.bits
      
  def jac(P : CurvePoint) = new Jacobian(P)
      
  def sign(hash:BigInt, fast:Boolean = false, sinv:Boolean = false):(BigInt,BigInt) = {
    assert(sec != null, "private key not set")
    val k = BigInt.apply(curve.q.bitLength+1, random) % n
    val r = if (fast)
      (jac(curve.P) * k).affine.x % n;
    else
      (curve.P * k).x % n;
    val s = if(sinv)
      (k * (hash + sec*r).modInverse(n)) % n      
    else 
      k.modInverse(n) * (hash + sec*r) % n
    return (r, s)
  }
  
  def verify(hash:BigInt, signature:(BigInt, BigInt), fast:Boolean = false, sinv:Boolean = false):Boolean = {
    val (r,s) = signature
    val w = if(sinv) s else s.modInverse(n);
    val u1 = hash * w % n;
    val u2 = r * w % n
    val X = if(fast)
      (jac(curve.P) * u1 + jac(pub) * u2).affine;
    else
      curve.P * u1 + pub * u2;
    val v = X.x % n
    v == r
  }
    
  override def toString =
    "x: " + pub.x.toString(16) + "\n" +
    "y: " + pub.y.toString(16) + (if(sec == null) "" else "\nsec: " + sec.toString(16))
}