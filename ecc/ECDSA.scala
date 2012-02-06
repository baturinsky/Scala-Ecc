package ecc

import java.security.MessageDigest
import encode.Base64;
import encode.Hex

object ECDSA {

  var defaultDigest = "SHA-256"
  var defaultCurve = p256
  
  lazy val big2 = BigInt.apply(2)
  lazy val big3 = BigInt.apply(3)
  
  def hex(foo:String) = Hex.decode(foo)
  
  def curve(q:String, n:String, a:String = "-3", b:String, G:String) = {
    val mod = hex(q)
    val ca = hex(a).mod(mod)
    val cb = hex(b)
    val (x,y) = unpackPoint(hex(G), mod, ca, cb)
    new Curve(mod, hex(n), ca, cb, x, y)
  }	    

  def unpackPoint(G:BigInt, q:BigInt, a:BigInt, b:BigInt) : (BigInt, BigInt) = {
    val len:Int = G.bitLength>>3<<3
    val g16 = G.toString(2)
    val b1 = G.testBit(len + 1)
    val b2 = G.testBit(len)
    val comp = b1 || b2;
    val len2 = if (!comp) len/2 else len  
    val cut = hex("1")<<len2
    if(comp) {
      val x = new BigIntMod(G % cut, q); 
      val root = (x**3 + x*a + b).root
      (x, if (b2) root._1 else root._2)
    } else
  	  ((G>>len2) % cut, G % cut)
  }
  
  def unpackPoint(G:BigInt, curve:Curve = defaultCurve) : CurvePoint = {
    val (x,y) = unpackPoint(G, curve.q, curve.a, curve.b)
    curve.point(x,y)
  }

  def packPoint(p:CurvePoint, curve:Curve = defaultCurve) : BigInt = 
    ((if (p.y%2 == 1) big3 else big2) << p.curve.bits) + p.x

  def packPublic64(key:Key) =
    Base64.encodeInteger(packPoint(key.pub, key.curve).bigInteger)

  def packPublicBytes(key:Key) =
    packPoint(key.pub, key.curve).bigInteger.toByteArray()
    
  def unpackPublic(Gs:String, curve:Curve = defaultCurve):Key = {
    val G = BigInt.apply(1,Base64.decode(Gs))
    val p = unpackPoint(G, curve)
    Key.fromPub((p.x, p.y), curve)
  }
    
  def hashAndLength(text:String, digest:String = defaultDigest) : (BigInt, Int) = {
    val md = MessageDigest.getInstance(digest)
    md.update(text.getBytes("UTF-8"))
    val h = md.digest()
    (BigInt.apply(1,h), md.getDigestLength())
  }  

  def hash(text:String, digest:String = defaultDigest) : BigInt = 
    hashAndLength(text,digest)._1 
  
  def hash64(text:String, digest:String = defaultDigest) : String = 
    Base64.encodeInteger(hash(text, digest).bigInteger)

  def hashBytes(text:String, digest:String = defaultDigest) : Array[Byte] = 
    hash(text, digest).bigInteger.toByteArray()
  
/*
 *   NB: in base64 form, key format is r, concatenated with INVERSE of s, as in SJCL library. 
 *   Odd, but saves inverse operation each verify.
 */
  
  def sign(key:Key, text:String, digest:String = defaultDigest) : String = {
    val h = hash(text, digest) 
    var (r,s) = key.sign(h, fast=true, sinv=true)
    Base64.encodeInteger(((r << key.curve.bits) + s).bigInteger);    
  }
  
  def verify(sig:String, key:Key, text:String, digest:String = defaultDigest) : Boolean = {
    val h = hash(text, digest)
    val signature = BigInt.apply(1,Base64.decode(sig))
    val r = signature >> key.curve.bits
    var s = signature % (BigInt.apply(1)<<key.curve.bits)
    key.verify(h, (r,s), fast=true, sinv=true)
  }
  
  
  lazy val p256 = curve(
    q = "FFFFFFFF 00000001 00000000 00000000 00000000 FFFFFFFF FFFFFFFF FFFFFFFF",
    n = "FFFFFFFF 00000000 FFFFFFFF FFFFFFFF BCE6FAAD A7179E84 F3B9CAC2 FC632551",
    b = "5AC635D8 AA3A93E7 B3EBBD55 769886BC 651D06B0 CC53B0F6 3BCE3C3E 27D2604B",
    G = "03 6B17D1F2 E12C4247 F8BCE6E5 63A440F2 77037D81 2DEB33A0 F4A13945 D898C296"
  )

  lazy val secp256r1 = p256

  lazy val p192 = curve(
    q = "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFFFF FFFFFFFF",
    n = "FFFFFFFF FFFFFFFF FFFFFFFF 99DEF836 146BC9B1 B4D22831",
    b = "64210519 E59C80E7 0FA7E9AB 72243049 FEB8DEEC C146B9B1",
    G = "03 188DA80E B03090F6 7CBF20EB 43A18800 F4FF0AFD 82FF1012"
  )
  
  lazy val secp192r1 = p192

  lazy val p160 = curve(
    q = "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF 7FFFFFFF",
    n = "01 00000000 00000000 0001F4C8 F927AED3 CA752257",
    b = "1C97BEFC 54BD7A8B 65ACF89F 81D4D4AD C565FA45",
    G = "02 4A96B568 8EF57328 46646989 68C38BB9 13CBFC82"
  )
  
  lazy val secp160r1 = p160
  
  lazy val p128 = curve(
    q = "FFFFFFFD FFFFFFFF FFFFFFFF FFFFFFFF",
    n = "FFFFFFFE 00000000 75A30D1B 9038A115",
    b = "E87579C1 1079F43D D824993C 2CEE5ED3",
    G = "03 161FF752 8B899B2D 0C28607C A52C5B86"
  )
  
  lazy val secp128r1 = p128
    
}