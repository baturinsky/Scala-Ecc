package ecc

import scala.util.Random

class BigIntMod(val n: BigInt, val mod: BigInt) {
    
  def inv = BigIntMod(n.modInverse(mod), mod)

  def unary_- = BigIntMod(mod-n, mod)

  def +(arg: BigInt) = BigIntMod(n + arg, mod)
  def -(arg: BigInt) = BigIntMod(n - arg, mod)
  def *(arg: BigInt) = BigIntMod((n * arg) % mod, mod)
  def /(arg: BigInt) = this * BigIntMod(arg, mod).inv
  
  def +(arg: BigIntMod) = BigIntMod(n + arg.n, mod)
  def -(arg: BigIntMod) = BigIntMod(n - arg.n, mod)
  def *(arg: BigIntMod) = BigIntMod((n * arg.n) % mod, mod)
  def /(arg: BigIntMod) = this * BigIntMod(arg.n, mod).inv

  def pow(arg: BigIntMod) = BigIntMod(n.modPow(arg.n, mod), mod)
  def pow(arg: BigInt) = BigIntMod(n.modPow(arg, mod), mod)
  def pow(arg: Int) = BigIntMod(n.modPow(BigInt(arg), mod), mod)  

  /* works only for mod%4==3 */

  def root: (BigIntMod, BigIntMod) = {
    assert(mod % 4 == 3)
    val a = pow((mod + 1) / 4)
    val p = (a, -a)
    if (a % 2 == 1)
      p
    else
      p.swap
  }

  def normalize: BigIntMod = BigIntMod(n % mod, mod)

  override def toString = normalize.n.toString(16)
}

object BigIntMod {
  def apply(n: BigInt, mod: BigInt) =
    if(n>=0)
      new BigIntMod(n % mod, mod)
    else
      new BigIntMod(n % mod + mod, mod)
  
  implicit def toBigInt(arg:BigIntMod): BigInt = arg.n   
}