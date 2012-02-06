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