package ecc

class CurvePoint(val curve:Curve, x1:BigInt, y1:BigInt) extends Field[CurvePoint] {
  override val order = curve.q
  val x= BigIntMod(x1, curve.q)  
  val y= BigIntMod(y1, curve.q) 
  
  lazy val a = curve.a
  lazy val b = curve.b
  
  def valid = y.pow(2) == x.pow(3) + x*a + b
    
  def inv = curve.point(x.n, -y.n)
        
  override def +(that:CurvePoint):CurvePoint = {
    val d = (that.y - y) / (that.x - x)
    val x3 = d * d - x - that.x
    val y3 = d * (x - x3) - y
    return curve.point(x3, y3)
  }
  
  override def x2:CurvePoint = {
    val d = (x * x * 3 + a) / (y * 2)
    val x3 = d * d - x * 2
    val y3 = d * (x-x3) - y
    return curve.point(x3,y3)
  }
    
  override def toString =
    "x: " + x.toString + "\ny: " + y.toString
    
  def compress: BigInt =
    x.n + (BigInt(if (y.testBit(0)) 3 else 2) << curve.bits)
}

