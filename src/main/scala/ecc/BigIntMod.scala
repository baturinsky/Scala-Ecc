package ecc

import scala.util.Random

class BigIntMod(bigInteger: BigInt, val mod: BigInt) extends BigInt(bigInteger.mod(mod).underlying) {

  def inv = BigIntMod(modInverse(mod), mod)

  override def unary_- = BigIntMod(super.unary_-, mod)

  override def +(that: BigInt) = BigIntMod(super.+(that), mod)

  override def -(that: BigInt) = BigIntMod(super.-(that), mod)

  override def *(that: BigInt) = BigIntMod(super.*(that) % mod, mod)

  def /(that: BigIntMod) = this * that.inv

  override def pow(p: Int) = BigIntMod(super.pow(p), mod)

  /* works right only if mod%4==3 */

  def root: (BigIntMod, BigIntMod) = {
    assert(mod % 4 == 3)
    val a = modPow((mod + 1) / 4, mod)
    val p = (BigIntMod(a, mod), BigIntMod(mod - a, mod))
    if (a % 2 == 1)
      p
    else
      p.swap
  }

  def normalize: BigIntMod = BigIntMod(this % mod, mod)

  override def toString = normalize.toString(16)
}

object BigIntMod {
  def apply(n: BigInt, m: BigInt) = new BigIntMod(n, m)
}