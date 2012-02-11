#Elliptic Curve Cryptography in Scala

This is a library for ECDSA signing, written completely in Scala.
Only extra lib used is Base64 encoding, written by Robert Harder in Java.

Tested to be compatible with sjclecc ECC library for javascript.

Does NOT use native encryption Java interface, which are forbidden in some place (like Google App).

Works about 30% faster than Bouncy Castle ECDSA implemention in Java.

I'm not a specialist in cryptography (or Scala), so probably I messed in some places. But testing so far showed it works as it should.

```scala
import ecc._

object Test {

  def main(args: Array[String]): Unit = {
    val text = "This text will be signed with secret, using UTF-8 encoding, SHA-256 hash and NIST p192 curve, and then verified"
    val secret = encode.Hex.decode("9bdde8fb7d18f5a3cf7698ee307650c4b05c74c5896eca00fc8af4355d54db7")
    val key = Key.fromSec(secret, ECDSA.p192)
    val sig = ECDSA.sign(key, text, "SHA-256")

    var verify = ECDSA.verify(sig, key, text)

    println(verify)
  }

}
```