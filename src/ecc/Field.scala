package ecc

trait Field[T <: Field[T]] { self:T =>

  val order : BigInt
  
  def x2:T
  
  def +(that:T):T

  /* recursive multiplication */ 
  
  def mulrec(n:BigInt) : T =
    if(n == 1)
      this
    else if(n == 2)
      this.x2
    else {
      val p = (this * (n/2)).x2
      if (n%2==0)
        p
      else
        p + this
    }       

  /* iterative multiplication */
  
   def * (n:BigInt): T= {
     (n.bitLength-2 to 0 by -1)
     .foldLeft(this)((a,b)=>if(n.testBit(b)) a.x2 +this else a.x2 )
   }

}