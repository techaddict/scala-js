package java.math

final class BigInteger private[math](_sign: Int, _digits: Array[Int]) extends Number with Comparable[BigInteger] {
	import UtilBigEndian._

	private[math] var sign: Int = _sign
  private[math] var digits: Array[Int] = _digits

	if (sign < -1 || sign > 1)
    throw new IllegalArgumentException(sign + "signum Should be either -1, +1, 0")

  def this(_a: String, radix: Int) = {
  	this(0, Array.empty[Int])
    if (_a.length == 0)
      throw new NumberFormatException("Zero length BigInteger")
    val a = _a.dropWhile(_ == '0')
    if (a != "") {
      // Doesn't Handle +123
      val sign = if (a.length > 0 && a(0) == '-') -1 else 1
      val startChar = if (sign == -1) 1 else 0
      val stringLength = a.length + (if (sign == -1) -1 else 0)

      val charsPerInt = BigInteger.digitFitInInt(radix)
      val topChars = stringLength % charsPerInt
      val bigRadixDigitsLength = (stringLength / charsPerInt) + (if (topChars != 0) 1 else 0)
      val digits = new Array[Int](bigRadixDigitsLength)
      val bigRadix = BigInteger.bigRadices(radix - 2)

      var ind = digits.length - 1
      var substrStart = startChar
      var substrEnd = startChar + (if (topChars == 0) charsPerInt else topChars)
      while (substrStart < a.length) {
        val bigRadixDigit = Integer.parseInt(a.substring(substrStart, substrEnd), radix)
        if (bigRadixDigit < 0)
          throw new NumberFormatException("Illegal Digit")
        // digits * bigRadix + bigRadixDigit
        // Mix these two
        val len = digits.length - 1 - ind
        val newDigit = inplaceMultiplyByInt(digits, len, bigRadix)
        digits(ind) = newDigit + inplaceAdd(digits, len, bigRadixDigit)
        ind -= 1
        substrStart = substrEnd
        substrEnd += charsPerInt
      }
      this.sign = sign
      this.digits = removeLeadingZeroes(digits)
    }
    else {
    	this.sign = 0
    	this.digits = Array(0)
    }
  }
  def this(a: String) = this(a, 10)

	final def signum = sign

	final def compareTo(that: BigInteger): Int = {
    if (this.signum == that.signum) {
      (this.signum: @annotation.switch) match {
        case 1 => compareArrays(this.digits, that.digits)
        case -1 => compareArrays(that.digits, this.digits)
        case 0 => 0
      }
    } else {
      if (this.signum > that.signum) 1
      else -1
    }
  }

  /**
	  * Returns this value as a `Long`.
	  * If the magnitude is too large, -1 will be returned.
	  */
  @inline def longValue(): Long = {
    val value =
      if (digits.length > 1) ((digits(1).toLong) << 32) | (digits(0) & 0xFFFFFFFFL)
      else (digits(0) & 0xFFFFFFFFL)
    if (digits.length < 3) sign * value else -1L
  }

  @inline override def byteValue(): scala.Byte = longValue.toByte
  @inline override def shortValue(): scala.Short = longValue.toShort
  /**
    * Returns this value as an `Int`.
    * If the magnitude is too large, -1 will be returned.
    */
  @inline def intValue(): Int = if (digits.length < 2) sign * digits(0) else -1

  // TODO These two
  @inline def floatValue(): scala.Float = longValue.toFloat
  @inline def doubleValue(): scala.Double = longValue.toDouble
}

object BigInteger {
	lazy val digitFitInInt = Array(-1, -1, 30, 19, 15, 13, 11, 11, 10, 9, 9, 8, 8, 8, 8, 7, 7,
    7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5)
  lazy val bigRadices = Array(-2147483648, 1162261467,1073741824, 1220703125, 362797056,
    1977326743, 1073741824, 387420489, 1000000000, 214358881, 429981696, 815730721,
    1475789056, 170859375, 268435456, 410338673, 612220032, 893871739, 1280000000,
    1801088541, 113379904, 148035889, 191102976, 244140625, 308915776, 387420489,
    481890304, 594823321, 729000000, 887503681, 1073741824, 1291467969, 1544804416,
    1838265625, 60466176)

  // /** Stores the maximum exponent for each radix from 0 - 36,
  //   * so that `radix.pow(DigitsPerLong)` fits into a positive Long value.
  //   *
  //   * Note that radices of 0 and 1 are not supported.
  //   */
  // private[bignum] val DigitsPerLong: Array[Int] = Array(
  //     -1, -1, 62, 39, 31, 27, 24, 22, 20, 19, 18, 18, 17, 17, 16, 16, 15, 15, 15,
  //     14, 14, 14, 14, 13, 13, 13, 13, 13, 13, 12, 12, 12, 12, 12, 12, 12, 12)
}

object UtilBigEndian {
	final def compareArrays(an: Array[Int], bn: Array[Int]): Int = {
    val al = an.length
    val bl = bn.length
    if (al < bl)
      return -1
    if (al > bl)
      return 1

    var i = 0
    while (i < al) {
      val av = an(i)
      val bv = bn(i)
      if (av != bv)
        return if ((av & 0xFFFFFFFFL) < (bv & 0xFFFFFFFFL)) -1 else 1
      i += 1
    }

    return 0
  }

  final def inplaceAdd(a: Array[Int], aSize: Int, addend: Int): Int = {
    var pos = a.length - 1
    var carry = addend & 0xFFFFFFFFL
    while (pos > a.length - 1 - aSize && carry != 0) {
      carry += a(pos) & 0xFFFFFFFFL
      a(pos) = carry.toInt
      pos -= 1
      carry >>>= 32
    }
    carry.toInt
  }

  final def inplaceMultiplyByInt(a: Array[Int], aSize: Int, factor: Int): Int = {
    var pos = a.length - 1
    var carry = 0L
    var cond = a.length - 1 - aSize
    while (pos > cond) {
      carry = (a(pos) & 0xFFFFFFFFL) * (factor & 0xFFFFFFFFL) + carry
      a(pos) = carry.toInt
      pos -= 1
      carry >>>= 32
    }
    carry.toInt
  }

  /** Returns the original array if unchanged. */
  final def removeLeadingZeroes(arr: Array[Int]): Array[Int] = {
    var i = 0
    var empty = -1
    var stop = false
    while (i < arr.length && stop == false) {
      if (arr(i) == 0)
        empty = i
      else
        stop = true
      i += 1
    }
    if (empty == -1) {
      arr
    } else {
      val newLen = arr.length - empty - 1
      val newArr = new Array[Int](newLen)
      scala.compat.Platform.arraycopy(arr, empty + 1, newArr, 0, newLen)
      newArr
    }
  }
}
