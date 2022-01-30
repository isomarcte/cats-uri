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
import java.nio.ByteBuffer

object PercentDecoder {

  private val PercentUnicodeCodePoint: Int = 0x25

  private def isOverlongByte2(a: Int): Boolean =
    a < 0xc2

  private def isOverlongByte3(a: Int, b: Int): Boolean =
    a == 0xe0 && b < 0xa0

  private def isOverlongByte4(a: Int, b: Int): Boolean =
    a == 0xf0 && b < 0x90

  // def decode(value: String): Either[String, String] = {
  //   val in: CharBuffer = CharBuffer.wrap(value)
  //   val out: CharBuffer = CharBuffer.allocate(value.length)
  //   val byteSequenceBuffer: Array[Char] = new Array(9)
  //   var expectingUtf8ByteSequence: Boolean = false
  //   var leadingByte: Int = 0
  //   var state: DecodeFSM = Empty
  //   var utf8ByteSequenceLen: Int = -1
  //   var utf8ByteSequencePosition: Int = -1
  //   var error: String = null

  //   def hexCharToByte(char: Char): Int =
  //     char match {
  //       case c if c >= '0' && c <= '9' => (c - '0')
  //       case c if c >= 'A' && c <= 'F' => 10 + (c - 'A')
  //       case c if c >= 'a' && c <= 'f' => 10 + (c - 'a')
  //       case _ =>
  //         error = s"Character represented by unicode codepoint ${char} is not a valid hex character."
  //         Int.MaxValue
  //     }

  //   def utf8ByteLengthFromNibble(b: Int): Int = {
  //     b match {
  //       case 0xe =>
  //         3
  //       case 0xf =>
  //         4
  //       case b if (b >> 2) == 3 =>
  //         2
  //       case b if (b >> 3) == 0 =>
  //         1
  //       case _ =>
  //         error = s"Found a percent encoded sequence but it does not corrispond to any valud UTF-8 leading byte."
  //         Int.MaxValue
  //     }
  //   }

  //   def utf8ByteLength(b: Int): Int =
  //     if (b < 0x80) {
  //       1
  //     } else if ((b & 0xe0) == 0xc0) {
  //       2
  //     } else if ((b & 0xf0) == 0xe0) {
  //       3
  //     } else if ((b & 0xf8) == 0xf0) {
  //       4
  //     } else {
  //       error = s"Found a percent encoded sequence but it does not corrispond to any valud UTF-8 leading byte."
  //       Int.MaxValue
  //     }

  //   def decodeUtf8BytesToCodePoint: Unit =
  //     utf8ByteSequenceLen match {
  //       case 2 =>
  //         out.put(((utf8ByteSequenceBuffer(0) & 0x1f) << 6) | (utf8ByteSequenceBuffer(1) & 0x3f))
  //       case 3 =>
  //         out.put(((utf8ByteSequenceBuffer(0) & 0x0f) << 12) | ((utf8ByteSequenceBuffer(1) & 0x3f) << 6) | (utf8ByteSequenceBuffer(2) & 0x3f))
  //       case 4 =>
  //         out.put(((utf8ByteSequenceBuffer(0) & 0x07) << 18) | ((utf8ByteSequenceBuffer(1) & 0x3f) << 12) | ((utf8ByteSequenceBuffer(2) & 0x3f) << 6) | (utf8ByteSequenceBuffer(3) & 0x3f))
  //       case n =>
  //         error = s"Attempting to decode ${n} bytes, but was expected 2, 3, or 4. This is a cats-uri bug."
  //     }

  //   while (in.hasRemaining() && (error eq null)) {
  //     val next: Int = in.get()
  //     if (expectingUtf8ByteSequence) {
  //       val hi: Int = hexCharToByte(next)
  //       leadingByte = hi << 4
  //       utf8ByteLengthFromNibble(hi) match {
  //         case 1 =>
  //           if (in.hasRemaining()) {
  //             out.put(leadingByte | hexCharToByte(in.get()).toChar)
  //           } else {
  //             error = "Error while decoding percent encoded UTF-8 byte sequence. Expected at least 1 more hexidecimal character, but reached end of input."
  //           }
  //         case 2 =>
  //           // Get 4 characters, the low order bits of the leading byte, the next '%' and the next byte.
  //           if (in.remaining >= 4) {
  //             in.get(byteSequenceBuffer, 0, 4)
  //             leadingByte | hexCharToByte(byteSequenceBuffer(0))
  //           } else {
  //             error = s"Error while decoding percent encoded UTF-8 byte sequence. Expected at least 4 more characters to complete UTF-8 byte sequence, but input only has ${in.remaining}."
  //           }
  //         case 4 =>
  //           utf8ByteSequenceLen = 4
  //           utf8ByteSequencePosition = 1
  //           state = ReadLeadingHexByteLow4
  //         case n =>
  //           utf8ByteSequenceLen = n
  //           utf8ByteSequencePosition = 1
  //           state = ReadLeadingHexByteLow
  //       }
  //     } else {
  //       next match {
  //         case '%' =>
  //           expectingUtf8ByteSequence = true
  //         case otherwise =>
  //           out.put(otherwise)
  //       }
  //     }
  //     state match {
  //       case Empty =>

  //       case _ =>
  //         val hi: Int = hexCharToByte(next)
  //         leadingByte = hi << 4
  //         utf8ByteLengthFromNibble(hi) match {
  //           case 1 =>
  //             state = ReadLeadingHexByteLow1
  //           case 4 =>
  //             utf8ByteSequenceLen = 4
  //             utf8ByteSequencePosition = 1
  //             state = ReadLeadingHexByteLow4
  //           case n =>
  //             utf8ByteSequenceLen = n
  //             utf8ByteSequencePosition = 1
  //             state = ReadLeadingHexByteLow
  //         }
  //       case ReadLeadingHexByteLow1 =>
  //         out.put(utf8ByteSequenceBuffer(0) | hexCharToByte(next))
  //         state = Empty
  //       case ReadLeadingHexByteLow =>
  //         utf8ByteSequenceBuffer(0) = utf8ByteSequenceBuffer(0) | hexCharToByte(next)
  //         state = ReadPercent
  //       case ReadLeadingHexByteLow4 =>
  //         hexCharToByte(next) match {
  //           case low if low >> 3 == 0 =>
  //             utf8ByteSequenceBuffer(0) = utf8ByteSequenceBuffer(0) | low
  //             state = ReadPercent
  //           case _ =>
  //             error = "Invalid UTF-8 leading byte. First 5 bits where 11111, which is not valid."
  //         }
  //       case ReadPercent =>
  //         next match {
  //           case PercentUnicodeCodePoint =>
  //             state = ReadHexByteHi
  //           case otherwise =>
  //             error = s"Expected '%' character to complete multibyte UTF-8 sequence, but got: ${otherwise}."
  //         }
  //       case ReadHexByteHi =>
  //         (hexCharToByte(next) << 4) match {
  //           case hi if (hi & 0xc0) == 0x80 =>
  //             // All non-leading UTF-8 bytes must be less than 0xc0
  //             utf8ByteSequenceBuffer(utf8ByteSequencePosition) = hi
  //           case otherwise =>
  //             error = s"Invalid non-leading UTF-8 byte value $otherwise (upper 4 bits only). All non-leading UTF-8 byte values must be < 0xc0."
  //         }
  //         state = ReadHexByteLow
  //       case ReadHexByteLow =>
  //         hexCharToByte(next) match {
  //           case Int.MaxValue =>
  //             ()
  //           case low =>
  //             utf8ByteSequenceBuffer(utf8ByteSequencePosition) = utf8ByteSequenceBuffer(utf8ByteSequencePosition) | low
  //             if (utf8ByteSequencePosition >= (utf8ByteSequenceLen - 1)) {
  //               // Completed byte sequence
  //               decodeUtf8BytesToCodePoint
  //               state = Empty
  //             } else {
  //               utf8ByteSequencePosition += 1
  //               state = ReadPercent
  //             }
  //         }
  //     }
  //   }

  //   if (error eq null) {
  //     state match {
  //       case Empty =>
  //         val pos: Int = out.position()
  //         out.flip
  //         Right(new String(out.array, 0, pos))
  //       case ReadLeadingHexByteHi | ReadHexByteHi =>
  //         Left("Reached end of String, but expected at least two more hexidecimal digits. Last character was '%'.")
  //       case ReadLeadingHexByteLow | ReadHexByteLow | ReadLeadingHexByteLow1 | ReadLeadingHexByteLow4 =>
  //         Left("Reached end of String, but expected at least one more hexidecimal digits. Read '%' followed by hex digit, but then String terminated.")
  //       case ReadPercent =>
  //         Left("Reached end of String, but expected more percent encoded values. Decoded partial multi-byte UTF-8 percent encoded sequence.")
  //     }
  //   } else {
  //     Left(s"Error at near index ${index - 1} in input String: ${error}")
  //   }
  // }

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
        if (isOverlongByte2(b)) {
          setError(s"Error encountered at index ${index - 3}. Leading UTF-8 byte implies a two byte sequence which should be represented by 1 byte. Overlong.")
          Int.MinValue
        } else {
          2
        }
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
              if (isOverlongByte3(a, b)) {
                error = s"Error at index ${index - 9}. UTF-8 byte sequence of 3 bytes is overlong."
              } else {
                buffer.put((a & 0xf) << 12 | (b & 0x3f) << 6 | c & 0x3f)
              }
            } else {
              error = s"Error at index ${index - 9}. Invalid UTF-8 byte encoding: ${a.toHexString} ${b.toHexString} ${c.toHexString}"
            }
          case 4 =>
            val b: Int = decodePercentByte
            val c: Int = decodePercentByte
            val d: Int = decodePercentByte
            if ((a & 0xf8) == 0xf0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80 && (d & 0xc0) == 0x80) {
              if (isOverlongByte4(a, b)) {
                error = s"Error at index ${index - 12}. UTF-8 byte sequence of 4 bytes is overlong."
              } else {
                buffer.put((a & 0x7) << 18 | (b & 0x3f) << 12 | (c & 0x3f) << 6 | d & 0x3f)
              }
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
