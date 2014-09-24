package ecc

class CurvePoint(val curve:Curve, x1:BigInt, y1:BigInt) extends Field[CurvePoint] {
  override val order = curve.q
  val x= BigIntMod(x1, curve.q)  
  val y= BigIntMod(y1, curve.q) 
  
  lazy val a = curve.a
  lazy val b = curve.b
  
  def valid = y.pow(2) == x.pow(3) + x*a + b
    
  def inv = curve.point(x, -y)
        
  override def +(that:CurvePoint):CurvePoint = {
    val d = (that.y - y) / (that.x - x)
    val x3 = d.pow(2) - x - that.x
    val y3 = d * (x - x3) - y
    return curve.point(x3, y3)
  }
  
  override def x2:CurvePoint = {
    val d = (x.pow(2) * 3 + a) / (y * 2)
    val x3 = d.pow(2) - x*2
    val y3 = d * (x-x3) - y
    return curve.point(x3,y3)
  }
    
  override def toString =
    "x: " + x.toString + "\ny: " + y.toString
    
  def pack =
    (BigInt(if(y % 2 == 1) 3 else 2)<<curve.q.bitLength) + x
}
