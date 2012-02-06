package ecc
import ecc._

class Jacobian(val curve:Curve, _x:BigInt=null, _y:BigInt=null, _z:BigInt=null) extends Field[Jacobian] {
  override val order = curve.q
  val X = new BigIntMod(_x, curve.q)
  val Y = new BigIntMod(_y, curve.q)
  val Z = if(_z==null) curve.l else new BigIntMod(_z, curve.q)
  val isIdentity = _y == null

  implicit def this(p:CurvePoint) =     
    this(p.curve, p.x, p.y)    
    
  implicit def affine:CurvePoint = {
    if(isIdentity)
    	return null
    Jacobian.count += 1    	
    var zi = Z.inv
    var zi2 = Z.inv**2;
    return curve.point(X*zi2, Y*zi2*zi);
  }
  
  def x2():Jacobian = { 
    val A = Y**2
    val B = X * A * 4
    val C = A**2 * 8
    val D =
      if(curve.minusthree){
        val ZZ = Z**2 
        (X + ZZ)*(X - ZZ)*3
      } else
        (X**2 * 3) + (Z**4 * curve.a)
    
    val X3 = D**2 - B*2 
    return new Jacobian(
      curve,
      X3,
      D * (B - X3) - C,
      Y * Z * 2
    )
  }
  
  def +(_that:Jacobian) : Jacobian = {
    val that = 
      if (_that.Z == 1)
        _that
      else {
        new Jacobian(_that.affine)
      }
    
    val A = Z**2
    val B = Z * A
    val C = that.X * A
    val D = that.Y * B
    val E = C - X
    val F = D - Y
    val G = E**2
    val H = G * E
    val I = X * G
    val X3 = F**2 - (H + I*2)
    return new Jacobian(
      curve,
      X3,
      F*(I - X3) - Y * H,
      Z * E
    )
  }        
}

object Jacobian {
  var count = 0
}
