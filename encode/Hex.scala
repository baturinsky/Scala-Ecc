package encode

object Hex {
  def decode(s:String) = 
    BigInt(s.replaceAll("[^-0-9a-fA-F]", ""), 16)

}