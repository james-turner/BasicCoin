import org.scalatest.{FlatSpec, Matchers}

class CheckBitPrefixSpec extends FlatSpec with Matchers {

  behavior of "BitChecker"

  it should "pass if prefix bits are correct" in {
    val mask: Byte = 0x03 // 6 bit prefix expected 00000011
    val value: Byte = 0x02 // 6 bits prefix set to 00000010

    BitChecker.validate(Array(mask), Array[Byte](value)) should be(true)
  }

  it should "fail bit check if prefix doesnt have correct number of bits" in {
    val mask: Byte = 0x01 // 7 bit prefix expected 00000001
    val value: Byte = 0x02 // 6 bits prefix set to 00000010

    BitChecker.validate(Array(mask), Array[Byte](value)) should be(false)
  }

  it should "ignore excess bytes in value" in {
    val mask: Byte = 0x02 // 6 bit prefix expected 00000011
    val value: Byte = 0x02 // 6 bits prefix set to 00000010

    BitChecker.validate(Array(mask), Array[Byte](value, value)) should be(false)
  }

  it should "show me what happens to zips" in {
    val byte: Byte = 0x01
    val v = Array[Byte](byte).zip(Array[Byte](byte,byte))
    v should be(Array[(Byte,Byte)]((byte,byte)))
  }


}

object BitChecker {
  def validate(mask: Array[Byte], value: Array[Byte]): Boolean = {
    mask.zip(value).map { case (m, v) =>
      (m | v) == m
    }.foldLeft(true)(_ && _)
  }

}
