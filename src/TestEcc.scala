import ecc._
import scala.util.Random

object TestEcc {

  def main(args: Array[String]): Unit = {
    val text = "This text will be signed with secret, using UTF-8 encoding, SHA-256 hash and NIST p192 curve, and then verified"

    println("Private key (randomly generated):")

    val secret = BigInt(192, new Random)
    val key = Key.sec(secret, ECDSA.p192)
    println(key)

    println("\nsignature (is in BigInt form. Use Base64 or any other option to convert it to string, if you want):")

    val sig = ECDSA.sign(key, text, "SHA-256")
    println(sig.toString(16))

    println("\npublic key in compresed form:")

    val pub = key.pub.compress
    println(pub.toString(16))

    println("\nuncompressed public key")

    val uncompressedKey = Key.pub(pub, ECDSA.p192)
    println(uncompressedKey)

    println("\nverification with uncompressed key (should be true):")

    val verify = ECDSA.verify(sig, uncompressedKey, text)
    println(verify)
  }

}