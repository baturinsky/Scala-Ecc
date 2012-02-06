package ecc

object Curve{
  def hex(s:String) =
    BigInt(s.replaceAll("[^-0-9a-fA-F]", ""), 16)
  
}

class Curve (val q:BigInt, val n:BigInt, val a:BigInt, val b:BigInt, x:BigInt, y:BigInt) {
  val P:CurvePoint = new CurvePoint(this, new BigIntMod(x, q), new BigIntMod(y, q))
  val bits = q.bitLength
  lazy val l:BigIntMod = new BigIntMod(1, q)
  lazy val minusthree = (a == q - 3) 
      
  def point(x:BigInt, y:BigInt) = 
    new CurvePoint(this, x, y)

  def point(xy:(BigInt, BigInt)) = 
    new CurvePoint(this, xy._1, xy._2)
  
  def point(x:String, y:String) = 
    new CurvePoint(this, Curve.hex(x), Curve.hex(y))
          
  override def toString =
    "q:  " + q.toString(16) + "\n" + 
    "n:  " + n.toString + "\n" +
    "a:  " + a.toString + "\n" +
    "b:  " + b.toString + "\n" +
    "x:  " + P.x.toString + "\n" +
    "y:  " + P.y.toString + "\n"

}
