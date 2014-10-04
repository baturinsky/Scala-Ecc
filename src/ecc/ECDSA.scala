package ecc

import java.security.MessageDigest

object ECDSA {

  lazy val defaultDigest = "SHA-256"
  lazy val defaultCurve = p256

  def hashAndLength(text: String, digest: String = defaultDigest): (BigInt, Int) = {
    val md = MessageDigest.getInstance(digest)
    md.update(text.getBytes("UTF-8"))
    val h = md.digest
    (BigInt(1, h), md.getDigestLength)
  }

  def hash(text: String, digest: String = defaultDigest): BigInt =
    hashAndLength(text, digest)._1

  /*
 *   NB: key format is r, concatenated with INVERSE of s, as in Stanford Javascript Crypto Library. 
 *   Odd, but saves inverse operation each verify.
 */

  def sign(key: Key, text: String, digest: String = defaultDigest): BigInt = {
    val h = hash(text, digest)
    val (r, s) = key.sign(h)
    ((r << key.curve.bits) + s)
  }

  def verify(signature: BigInt, key: Key, text: String, digest: String = defaultDigest): Boolean = {
    val h = hash(text, digest)
    val r = signature >> key.curve.bits
    val s = signature % (BigInt(1) << key.curve.bits)
    key.verify(h, (r, s))
  }

  lazy val p256, secp256r1 = Curve(
    q = "FFFFFFFF 00000001 00000000 00000000 00000000 FFFFFFFF FFFFFFFF FFFFFFFF",
    n = "FFFFFFFF 00000000 FFFFFFFF FFFFFFFF BCE6FAAD A7179E84 F3B9CAC2 FC632551",
    b = "5AC635D8 AA3A93E7 B3EBBD55 769886BC 651D06B0 CC53B0F6 3BCE3C3E 27D2604B",
    G = "03 6B17D1F2 E12C4247 F8BCE6E5 63A440F2 77037D81 2DEB33A0 F4A13945 D898C296")

  lazy val p192, secp192r1 = Curve(
    q = "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFFFF FFFFFFFF",
    n = "FFFFFFFF FFFFFFFF FFFFFFFF 99DEF836 146BC9B1 B4D22831",
    b = "64210519 E59C80E7 0FA7E9AB 72243049 FEB8DEEC C146B9B1",
    G = "03 188DA80E B03090F6 7CBF20EB 43A18800 F4FF0AFD 82FF1012")

  lazy val p160, secp160r1 = Curve(
    q = "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF 7FFFFFFF",
    n = "01 00000000 00000000 0001F4C8 F927AED3 CA752257",
    b = "1C97BEFC 54BD7A8B 65ACF89F 81D4D4AD C565FA45",
    G = "02 4A96B568 8EF57328 46646989 68C38BB9 13CBFC82")

  lazy val p128, secp128r1 = Curve(
    q = "FFFFFFFD FFFFFFFF FFFFFFFF FFFFFFFF",
    n = "FFFFFFFE 00000000 75A30D1B 9038A115",
    b = "E87579C1 1079F43D D824993C 2CEE5ED3",
    G = "03 161FF752 8B899B2D 0C28607C A52C5B86")

}