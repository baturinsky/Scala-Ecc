package ecc

import scala.math.BigInt
import java.math.BigInteger
import scala.util.Random

object BigIntMod{
  lazy val bio = new BigInteger("0")
}

class BigIntMod(bigInteger:BigInteger, val mod:BigInt) extends BigInt(bigInteger.mod(mod.bigInteger)) {
    
  def this(n: BigInt, mod: BigInt) = 
    this(if(n!=null) n.bigInteger else BigIntMod.bio, mod)
    
  def bimr(n: BigInteger) = 
    new BigIntMod(n, mod)
  
  def bim(n: BigInt) = 
    new BigIntMod(n.bigInteger, mod)
  
  def inv = 
  	new BigIntMod(modInverse(mod), mod)
  
  override def unary_- =  
  	bim(super.unary_-)
  
  override def + (that : BigInt) = 
    bim(super.+(that))

  override def - (that : BigInt) = 
    bim(super.-(that))
  	
  override def * (that : BigInt) = 
    bim(super.*(that)%mod)

  def / (that : BigIntMod) = 
    this * that.inv
    
  def ** (power:Integer):BigIntMod =
    if(power == 1)
      this
    else if(power == 2)
      this * this
    else if(power == 3)
      this * this * this
    else {
      val p = (this ** (power/2))**2
      if (power%2==0)
        p
      else
        p * this
    }
  
  /* works right only if mod%4==3 */
  
  def root : (BigIntMod, BigIntMod) = {
    assert(mod%4 == 3)
    val a = modPow((mod+1)/4, mod)
    val p = (bim(a), bim(mod-a))
    if (a%2 == 1)
      p
    else
      p.swap
  }

  implicit def int:BigInt = 
    this % mod
    
  def normalize:BigIntMod = 
    new BigIntMod(int%mod, mod)
              
  override def toString =
    int.toString(16)
  
}