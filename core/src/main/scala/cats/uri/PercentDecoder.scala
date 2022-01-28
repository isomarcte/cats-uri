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

  private def utf8ByteLength(b: Int): Either[String, Int] =
    if ((b & 0x80) == 0) {
      Right(1)
    } else if ((b & 0xe0) == 0xc0) {
      Right(2)
    } else if ((b & 0xf0) == 0xe0) {
      Right(3)
    } else if ((b & 0xf8) == 0xf0) {
      Right(4)
    } else {
      Left("The given byte is not a valid leading byte in a UTF-8 byte sequence.")
    }

  private def hexCodePointToInt(codePoint: Int): Either[String, Int] =
    codePoint match {
      case c if c >= '0' && c <= '9' => Right((c - '0'))
      case c if c >= 'A' && c <= 'F' => Right(10 + (c - 'A'))
      case c if c >= 'a' && c <= 'f' => Right(10 + (c - 'a'))
      case _ =>
        Left(s"Character reprsented by unicode codepoint ${codePoint} is not a valid hex character.")
    }

  def hexCodePointsToByte(hi: Int, low: Int): Either[String, Int] =
    (hexCodePointToInt(hi), hexCodePointToInt(low)).mapN{
      case (hi, low) =>
        ((hi << 4) | low)
    }

  // /**
  //  * Parse of sequence of bytes as a UTF-8 encoded `String`.
  //  *
  //  * Why not use `new String(bytes, "UTF-8")`? `new String` will replace
  //  * invalid byte sequences with the Unicode replacement character. This is
  //  * the recommended way to handle invalid byte sequences by the Unicode
  //  * specification, but in the specific sub-domain of URI parsing is not what
  //  * we want. Replacing invalid byte sequences creates ambiguity when
  //  * attempting to encode a URI with an ''intentionally'' encoded Unicode
  //  * replacement character, \ufffd. This effectively changes the Uri. Further,
  //  * other implementations also yield errors on invalid byte sequences.
  //  *
  //  * {{{
  //  * > decodeURIComponent("%F4%90%BF%BF")
  //  * Uncaught URIError: URI malformed
  //  *   at decodeURIComponent (<anonymous>)
  //  * }}}
  //  */
  // private def utfBytesToString(nel: NonEmptyList[Byte]): Either[String, String] = {
  //   val builder: StringBuilder =
  //     new StringBuilder(nel.size * 2)


  //   def decodeToCodePoint(bytes: List[Byte]): Either[String, Int] =
  //     bytes match {
  //       case a :: Nil =>
  //         if ((a & 0x80) == 0) {
  //           Right(a.toInt)
  //         } else {
  //           Left(s"Invalid UTF-8 byte encoding. Characters of represented by 1 byte must by < 0x80: ${a}")
  //         }
  //       case a :: b :: Nil =>
  //         if ((a & 0xe0) == 0xc0 && (b & 0xc0) == 0x80) {
  //           Right((a & 0x1f) << 6 | b & 0x3f)
  //         } else {
  //           Left(s"Invalid UTF-8 byte encoding. For characters represented by 2 bytes, the first byte must be < 0xe0 and the second by must be < 0xc0: ${a} ${b}")
  //         }
  //       case a :: b :: c :: Nil =>
  //         if ((a & 0xf0) == 0xe0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80) {
  //           Right((a & 0xf) << 12 | (b & 0x3f) << 6 | c & 0x3f)
  //         } else {
  //           Left(s"Invalid UTF-8 byte encoding. For characters represented by 3 bytes, the first byte must be < 0xf0 and the second and third by must be < 0xc0: ${a} ${b} ${c}")
  //         }
  //       case a :: b :: c :: d :: Nil =>
  //         if ((a & 0xf8) == 0xf0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80 && (d & 0xc0) == 0x80) {
  //           Right((a & 0x7) << 18 | (b & 0x3f) << 12 | (c & 0x3f) << 6 | d & 0x3f)
  //         } else {
  //           Left(s"Invalid UTF-8 byte encoding. For characters represented by 4 bytes, the first byte must be < 0xf8 and the second and third by must be < 0xc0: ${a} ${b} ${c}")
  //         }
  //       case _ =>
  //         throw new AssertionError(s"Invalid byte sequence length for UTF-8: ${bytes.size}. This is a cats-uri bug.")
  //     }

  //   @tailrec
  //   def loop(head: Byte, tail: List[Byte]): Either[String, String] = {
  //     val characterLength: Int = utf8ByteLength(head)

  //     if (tail.size < (characterLength - 1)) {
  //       Left(s"Invalid UTF-8 byte sequence in percent encoded value. Expected at least ${characterLength} more bytes, but only had ${tail.size + 1}")
  //     } else {
  //       tail.splitAt(characterLength - 1) match {
  //         case (chars, next) =>
  //           decodeToCodePoint(head :: chars) match {
  //             case Right(codePoint) =>
  //               builder.appendCodePoint(codePoint)
  //               next match {
  //                 case Nil =>
  //                   Right(builder.toString)
  //                 case head :: next =>
  //                   loop(head, next)
  //               }
  //             case Left(e) =>
  //               Left(e)
  //           }
  //       }
  //     }
  //   }

  //   loop(nel.head, nel.tail)
  // }

  /**
   * Percent decode a given `String`.
   */
  def decode(value: String): Either[String, String] = {
    val len: Int = value.length
    val buffer: IntBuffer = IntBuffer.allocate(value.codePointCount(0, len))
    val miniBuffer: IntBuffer = IntBuffer.allocate(3)
    var error: String = null
    var index: Int = 0

    def setError(e: String): Unit =
      if (error eq null) {
        error = e
      } else {
        ()
      }

    def decodeByte1(a: Int): Int =
      if ((a & 0x80) == 0) {
        a.toInt
      } else {
        setError(s"Error at index ${index - 3}. Invalid UTF-8 byte encoding: ${a.toHexString}")
        Int.MinValue
      }

    def decodeByte2(a: Int, b: Int): Int =
      if ((a & 0xe0) == 0xc0 && (b & 0xc0) == 0x80) {
        (a & 0x1f) << 6 | b & 0x3f
      } else {
        setError(s"Error at index ${index - 6}. Invalid UTF-8 byte encoding: ${a.toHexString} ${b.toHexString}")
        Int.MinValue
      }

    def decodeByte3(a: Int, b: Int, c: Int): Int =
      if ((a & 0xf0) == 0xe0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80) {
        (a & 0xf) << 12 | (b & 0x3f) << 6 | c & 0x3f
      } else {
        setError(s"Error at index ${index - 9}. Invalid UTF-8 byte encoding: ${a.toHexString} ${b.toHexString} ${c.toHexString}")
        Int.MinValue
      }

    def decodeByte4(a: Int, b: Int, c: Int, d: Int): Int =
      if ((a & 0xf8) == 0xf0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80 && (d & 0xc0) == 0x80) {
        (a & 0x7) << 18 | (b & 0x3f) << 12 | (c & 0x3f) << 6 | d & 0x3f
      } else {
        setError(s"Erorr at index ${index - 12}. Invalid UTF-8 byte encoding: ${a.toHexString} ${b.toHexString} ${c.toHexString}")
        Int.MinValue
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
            val codePoint: Int = decodeByte1(a)
            if (error eq null) {
              buffer.put(codePoint)
            } else {
              ()
            }
          case 2 =>
            val codePoint: Int = decodeByte2(a, decodePercentByte)
            if (error eq null) {
              buffer.put(codePoint)
            } else {
              ()
            }
          case 3 =>
            val codePoint: Int = decodeByte3(a, decodePercentByte, decodePercentByte)
            if (error eq null) {
              buffer.put(codePoint)
            } else {
              ()
            }
          case 4 =>
            val codePoint: Int = decodeByte4(a, decodePercentByte, decodePercentByte, decodePercentByte)
            if (error eq null) {
              buffer.put(codePoint)
            } else {
              ()
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
