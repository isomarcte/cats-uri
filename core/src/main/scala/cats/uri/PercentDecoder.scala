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

    def decodeByte1(a: Int): Either[String, Int] =
      if ((a & 0x80) == 0) {
        Right(a.toInt)
      } else {
        Left(s"Invalid UTF-8 byte encoding. Characters of represented by 1 byte must by < 0x80: ${a}")
      }

    def decodeByte2(a: Int, b: Int): Either[String, Int] =
      if ((a & 0xe0) == 0xc0 && (b & 0xc0) == 0x80) {
        Right((a & 0x1f) << 6 | b & 0x3f)
      } else {
        Left(s"Invalid UTF-8 byte encoding. For characters represented by 2 bytes, the first byte must be < 0xe0 and the second by must be < 0xc0: ${a} ${b}")
      }

    def decodeByte3(a: Int, b: Int, c: Int): Either[String, Int] =
      if ((a & 0xf0) == 0xe0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80) {
        Right((a & 0xf) << 12 | (b & 0x3f) << 6 | c & 0x3f)
      } else {
        Left(s"Invalid UTF-8 byte encoding. For characters represented by 3 bytes, the first byte must be < 0xf0 and the second and third by must be < 0xc0: ${a} ${b} ${c}")
      }

    def decodeByte4(a: Int, b: Int, c: Int, d: Int): Either[String, Int] =
      if ((a & 0xf8) == 0xf0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80 && (d & 0xc0) == 0x80) {
        Right((a & 0x7) << 18 | (b & 0x3f) << 12 | (c & 0x3f) << 6 | d & 0x3f)
      } else {
        Left(s"Invalid UTF-8 byte encoding. For characters represented by 4 bytes, the first byte must be < 0xf8 and the second and third by must be < 0xc0: ${a} ${b} ${c}")
      }

    def extractPercentHexBytes(
      count: Int,
      cursor: Int
    ): Either[String, Unit] = {
      if (count <= 0) {
        Right(())
      } else if (cursor + 2 >= len) {
        Left(s"Encountered an incomplete percent encoded UTF-8 byte sequence at index ${cursor - 3} of the given String.")
      } else {
        value.codePointAt(cursor) match {
          case PercentUnicodeCodePoint =>
            value.codePointAt(cursor + 1) match {
              case hi if hi <= 'z'.toInt =>
                val low: Int = value.codePointAt(cursor + 2)
                hexCodePointsToByte(hi, low) match {
                  case Right(byte) =>
                    miniBuffer.put(byte)
                    extractPercentHexBytes(count - 1, cursor + 3)
                  case Left(e) =>
                    Left(e)
                }
              case otherwise =>
                Left(s"Encountered an invalid percent encoded sequence at index ${cursor}. Codepoint ${otherwise} does not represent a valid hexidecimal character.")
            }
          case otherwise =>
            Left(s"Expected '%' when decoding multibyte UTF-8 byte sequence at index ${cursor - 3}, got codepoint ${otherwise.toHexString}.")
        }
      }
    }

    def extractPercentHexByteSequence(
      cursor: Int
    ): Either[String, Int] =
      value.codePointAt(cursor) match {
        case hi if hi <= 'z'.toInt =>
          val low: Int = value.codePointAt(cursor + 1)
          hexCodePointsToByte(hi, low) match {
            case Right(byte) =>
              utf8ByteLength(byte) match {
                case Right(1) =>
                  decodeByte1(byte) match {
                    case Right(codePoint) =>
                      buffer.put(codePoint)
                      Right(cursor + 2)
                    case Left(e) => Left(e)
                  }
                case Right(2) =>
                  miniBuffer.clear()
                  extractPercentHexBytes(1, cursor + 2)
                  decodeByte2(byte, miniBuffer.get(0)) match {
                    case Right(codePoint) =>
                      buffer.put(codePoint)
                      Right(cursor + 2 + 3)
                    case Left(e) => Left(e)
                  }
                case Right(3) =>
                  miniBuffer.clear()
                  extractPercentHexBytes(2, cursor + 2)
                  decodeByte3(byte, miniBuffer.get(0), miniBuffer.get(1)) match {
                    case Right(codePoint) =>
                      buffer.put(codePoint)
                      Right(cursor + 2 + 6)
                    case Left(e) => Left(e)
                  }
                case Right(4) =>
                  miniBuffer.clear()
                  extractPercentHexBytes(3, cursor + 3)
                  decodeByte4(byte, miniBuffer.get(0), miniBuffer.get(1), miniBuffer.get(2)) match {
                    case Right(codePoint) =>
                      buffer.put(codePoint)
                      Right(cursor + 2 + 9)
                    case Left(e) => Left(e)
                  }
                case Left(e) =>
                  Left(e)
                case Right(invalid) =>
                  throw new AssertionError(s"Supposed UTF-8 leading byte ${byte.toHexString} indicated a byte length for the UTF-8 codepoint of ${invalid}, but that is not possible. This is a cats-uri bug.")
              }
            case Left(e) => Left(e)
          }
        case otherwise =>
          Left(s"Encountered an invalid percent encoded sequence at index ${cursor}. Codepoint ${otherwise} does not represent a valid hexidecimal character.")
      }

    @tailrec
    def loop(index: Int): Either[String, String] =
      if (index >= len) {
        val count: Int = buffer.position()
        buffer.flip
        Right(new String(buffer.array, 0, count))
      } else {
        val codePoint: Int = value.codePointAt(index)

        codePoint match {
          case PercentUnicodeCodePoint =>
            if (index + 2 >= len) {
              Left(s"Encountered unterminated % sequence at index ${index} of the String. Expected a % followed by two hexidecimal digits.")
            } else {
              extractPercentHexByteSequence(index + 1) match {
                case Right(next) =>
                  loop(next)
                case Left(e) =>
                  Left(e)
              }
            }
          case rawCodePoint =>
            buffer.put(rawCodePoint)
            val inc: Int = if (rawCodePoint >= 0x10000) 2 else 1
            loop(index + inc)
        }
      }

    loop(0)
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
