package ecc

class CurvePoint(val curve:Curve, _x:BigInt, _y:BigInt) extends Field[CurvePoint] {
  override val order = curve.q
  val x:BigIntMod = new BigIntMod(_x, curve.q)  
  val y:BigIntMod = new BigIntMod(_y, curve.q) 
  
  lazy val a = curve.a
  lazy val b = curve.b
  
  def valid =
    y**2 == x**3 + x*a + b
    
  def inv = 
    curve.point(x, -y)
        
  override def +(that:CurvePoint):CurvePoint = {
    var d = (that.y - y) / (that.x - x)
    var x3 = d**2 - x - that.x
    var y3 = d * (x - x3) - y
    CurvePoint.sums += 1
    return curve.point(x3, y3)
  }
  
  override def x2:CurvePoint = {
    var d = (x**2 * 3 + a) / (y * 2)
    var x3 = d**2 - x*2
    var y3 = d * (x-x3) - y
    CurvePoint.x2s += 1
    return curve.point(x3,y3)
  }
    
  override def toString =
    "x: " + x.toString + "\ny: " + y.toString
    
  def pack =
    (BigInt.apply(if(y % 2 == 1) 3 else 2)<<curve.q.bitLength) + x
}

object CurvePoint{
  var sums = 0
  var x2s = 0
}