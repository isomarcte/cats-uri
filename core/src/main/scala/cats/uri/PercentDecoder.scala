package cats.uri

import cats.syntax.all._
import cats.data._
import java.nio.CharBuffer
import java.nio.charset.CharsetEncoder
import java.lang.StringBuilder
import cats.parse._
import cats.uri.parsers._
import scala.annotation.tailrec
import java.nio.charset.StandardCharsets
import java.nio.IntBuffer

object PercentDecoder {

  private val PercentUnicodeCodePoint: Int = 0x25

  /**
   * Percent decode a given `String`.
   */
  def decode(value: String): Either[String, String] = {
    val len: Int = value.length
    val buffer: IntBuffer = IntBuffer.allocate(value.codePointCount(0, len))
    var error: String = null
    var index: Int = 0

    def setError(e: String): Unit =
      if (error eq null) {
        error = e
      } else {
        ()
      }

    def utf8ByteLength(b: Int): Int =
      if ((b & 0x80) == 0) {
        1
      } else if ((b & 0xe0) == 0xc0) {
        2
      } else if ((b & 0xf0) == 0xe0) {
        3
      } else if ((b & 0xf8) == 0xf0) {
        4
      } else {
        setError(s"Error encountered at index ${index - 3}. Found a percent encoded sequence but it does not corrispond to any valud UTF-8 leading byte.")
        Int.MinValue
      }

    def hexCodePointToInt(codePoint: Int): Int =
      codePoint match {
        case c if c >= '0' && c <= '9' => (c - '0')
        case c if c >= 'A' && c <= 'F' => 10 + (c - 'A')
        case c if c >= 'a' && c <= 'f' => 10 + (c - 'a')
        case _ =>
          setError(s"Error encountered at ${index}. Character represented by unicode codepoint ${codePoint} is not a valid hex character.")
          Int.MinValue
      }

    // Read two codePoints and handle the index bookeeping.
    def readNextTwoCodePointsAsHexByte: Int = {
      if (index + 1 < len) {
        val hi: Int = hexCodePointToInt(value.codePointAt(index))
        val low: Int = hexCodePointToInt(value.codePointAt(index + 1))
        if (error eq null) {
          index += 2
          ((hi << 4) | low)
        } else {
          Int.MinValue
        }
      } else {
        setError(s"Error encountered at ${index}. Expected at least two more characters to decode percent encoded value, but only found ${index - len}")
        Int.MinValue
      }
    }

    def decodePercentByte: Int =
      if (index + 2 < len) {
        value.codePointAt(index) match {
          case PercentUnicodeCodePoint =>
            index += 1
            readNextTwoCodePointsAsHexByte
          case otherwise =>
            setError(s"Error encountered at ${index}. Expected at least one more percent encoded UTF-8 byte sequence, but next codepoint was not a '%': ${otherwise}")
            Int.MinValue
        }
      } else {
        setError(s"Error encountered at ${index}. Expected at least one more percent encoded UTF-8 byte sequence, but reached the end of the String.")
        Int.MinValue
      }

    def extractPercentHexByteSequence: Unit = {
      val a: Int = readNextTwoCodePointsAsHexByte
      if (error eq null) {
        utf8ByteLength(a) match {
          case 1 =>
            if ((a & 0x80) == 0) {
              buffer.put(a)
            } else {
              error = s"Error at index ${index - 3}. Invalid UTF-8 byte encoding: ${a.toHexString}"
            }
          case 2 =>
            val b: Int = decodePercentByte
            if ((a & 0xe0) == 0xc0 && (b & 0xc0) == 0x80) {
              buffer.put((a & 0x1f) << 6 | b & 0x3f)
            } else {
              error = s"Error at index ${index - 6}. Invalid UTF-8 byte encoding: ${a.toHexString} ${b.toHexString}"
            }
          case 3 =>
            val b: Int = decodePercentByte
            val c: Int = decodePercentByte
            if ((a & 0xf0) == 0xe0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80) {
              buffer.put((a & 0xf) << 12 | (b & 0x3f) << 6 | c & 0x3f)
            } else {
              error = s"Error at index ${index - 9}. Invalid UTF-8 byte encoding: ${a.toHexString} ${b.toHexString} ${c.toHexString}"
            }
          case 4 =>
            val b: Int = decodePercentByte
            val c: Int = decodePercentByte
            val d: Int = decodePercentByte
            if ((a & 0xf8) == 0xf0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80 && (d & 0xc0) == 0x80) {
              buffer.put((a & 0x7) << 18 | (b & 0x3f) << 12 | (c & 0x3f) << 6 | d & 0x3f)
            } else {
              error =s"Erorr at index ${index - 12}. Invalid UTF-8 byte encoding: ${a.toHexString} ${b.toHexString} ${c.toHexString}"
            }
          case _ =>
            // Should already be set
            setError(s"Error encountered at index ${index - 3}. Found a percent encoded sequence but it does not corrispond to any valud UTF-8 leading byte.")
        }
      } else {
        ()
      }
    }

    while(index < len && (error eq null)) {
      value.codePointAt(index) match {
        case PercentUnicodeCodePoint =>
          // '%' is always only 1 character in the String
          index += 1

          // Check to ensure there are at least 2 more character for the
          // expected 2 hexidecimal character.
          if (index + 1 >= len) {
            error = s"Encountered unterminated % sequence at index ${index} of the String. Expected a % followed by two hexidecimal digits."
          } else {
            // Index bookeeping is handled in this function
            extractPercentHexByteSequence
          }
        case codePoint =>
          // Does not need decoding, goes directly into the buffer.
          buffer.put(codePoint)

          // Unicode codepoints >= 0x10000 are represnted by two characters in
          // the String, (value: String).codePointAt(index) accounts for this,
          // but we need to increment our index by 2 in this case. Otherwise
          // we would read the second character of a surrogate pair, which is
          // not what we want.
          val inc: Int = if (codePoint >= 0x10000) 2 else 1
          index += inc
        }
      }

    if (error eq null) {
      val count: Int = buffer.position()
      buffer.flip
      Right(new String(buffer.array, 0, count))
    } else {
      Left(error)
    }
  }
  // (percentByteSubStringParser | (Parser.anyChar *> Parser.charsWhile0(_ != '%')).string).repAs0[String].parseAll(value).fold(
  //   e => Left(s"Decoding the percent encoded value failed. This likely means there was an invalid percent encoded byte sequence or you have a literal '%' in the String which does not map to a percent encoded UTF-8 byte sequence"),
  //   value => Right(value)
  // )

  def unsafeDecode(value: String): String =
    decode(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
