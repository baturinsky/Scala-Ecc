package ecc

import scala.util.Random

class BigIntMod(val n: BigInt, val mod: BigInt) {

  import BigIntMod.trueMod

  def inv = new BigIntMod(n.modInverse(mod), mod)

  def unary_- = new BigIntMod(mod - n, mod)

  def +(arg: BigInt) = BigIntMod(n + arg, mod)
  def -(arg: BigInt) = BigIntMod(n - arg, mod)
  def *(arg: BigInt) = BigIntMod(n * arg, mod)
  def /(arg: BigInt) = this * BigIntMod(arg, mod).inv

  def +(arg: BigIntMod) = BigIntMod(n + arg.n, mod)
  def -(arg: BigIntMod) = BigIntMod(n - arg.n, mod)
  def *(arg: BigIntMod) = BigIntMod(n * arg.n, mod)
  def /(arg: BigIntMod) = this * BigIntMod(arg.n, mod).inv

  def ==(arg: BigInt): Boolean = trueMod(arg, mod) == n
  def ==(arg: BigIntMod): Boolean = trueMod(arg.n, mod) == n

  def pow(arg: BigIntMod) = BigIntMod(n.modPow(arg.n, mod), mod)
  def pow(arg: BigInt) = BigIntMod(n.modPow(arg, mod), mod)
  def pow(arg: Int) = BigIntMod(n.modPow(BigInt(arg), mod), mod)

  /* works only for mod%4==3 */

  def root: (BigIntMod, BigIntMod) = {
    assert(mod % 4 == 3)
    val a = pow((mod + 1) / 4)
    val p = (a, -a)
    if (a.testBit(0))
      p
    else
      p.swap
  }

  override def toString = "n: " + n.toString(16) + " mod:" + mod.toString(16)
}

object BigIntMod {

  def trueMod(n: BigInt, mod: BigInt) =
    if (n >= 0)
      n % mod
    else
      n % mod + mod

  def apply(n: BigInt, mod: BigInt) =
    new BigIntMod(trueMod(n, mod), mod)

  implicit def toBigInt(arg: BigIntMod): BigInt = arg.n
}