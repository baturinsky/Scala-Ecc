package ecc

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Protocol_specification_SignaturesSpec extends FreeSpec {

  val text = "This text will be signed with secret, using UTF-8 encoding, SHA-256 hash and NIST p192 curve, and then verified"
  val secret = BigInt("9bdde8fb7d18f5a3cf7698ee307650c4b05c74c5896eca00fc8af4355d54db7",16)

  text - {

    val key = Key(secret, ECDSA.p192)
    val sig = ECDSA.sign(key, text, "SHA-256")

    " #2: " + secret in {
      val verify = ECDSA.verify(sig, key, text)
      assert(verify)
    }
  }

}