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

object PercentDecoder {
  private def hexCharToInt(c: Char): Int =
    c match {
      case c if c >= '0' && c <= '9' => (c - '0')
      case c if c >= 'A' && c <= 'F' => 10 + (c - 'A')
      case c if c >= 'a' && c <= 'f' => 10 + (c - 'a')
      case _ =>
        throw new AssertionError(s"Can not interpret char $c as a hex int. This is a cats-uri bug.")
    }

  private val percentByteParser: Parser[Byte] =
    Rfc3986.percentEncoded.map{
      case (a, b) =>
        ((hexCharToInt(a) << 4) | hexCharToInt(b)).toByte
    }

  /**
   * Parse of sequence of bytes as a UTF-8 encoded `String`.
   *
   * Why not use `new String(bytes, "UTF-8")`? `new String` will replace
   * invalid byte sequences with the Unicode replacement character. This is
   * the recommended way to handle invalid byte sequences by the Unicode
   * specification, but in the specific sub-domain of URI parsing is not what
   * we want. Replacing invalid byte sequences creates ambiguity when
   * attempting to encode a URI with an ''intentionally'' encoded Unicode
   * replacement character, \ufffd. This effectively changes the Uri. Further,
   * other implementations also yield errors on invalid byte sequences.
   *
   * {{{
   * > decodeURIComponent("%F4%90%bf%bf")
   * Uncaught URIError: URI malformed
   *   at decodeURIComponent (<anonymous>)
   * }}}
   */
  private def utfBytesToString(nel: NonEmptyList[Byte]): Either[String, String] = {
    val builder: StringBuilder =
      new StringBuilder(nel.size * 2)

    def utf8ByteLength(b: Byte): Int =
    if ((b & 0xe0) == 0xc0) {
      2
    } else if ((b & 0xf0) == 0xe0) {
      3
    } else if ((b & 0xf8) == 0xf0) {
      4
    } else {
      // This might also match invalid UTF-8 bytes, but that's fine. Such
      // bytes will fail during decoding.
      1
    }

    def decodeToCodePoint(bytes: List[Byte]): Either[String, Int] =
    bytes match {
      case a :: Nil =>
        if ((a & 0x80) == 0) {
          Right(a.toInt)
        } else {
          Left(s"Invalid UTF-8 byte encoding. Characters of represented by 1 byte must by < 0x80: ${a}")
        }
      case a :: b :: Nil =>
        if ((a & 0xe0) == 0xc0 && (b & 0xc0) == 0x80) {
          Right((a & 0x1f) << 6 | b & 0x3f)
        } else {
          Left(s"Invalid UTF-8 byte encoding. For characters represented by 2 bytes, the first byte must be < 0xe0 and the second by must be < 0xc0: ${a} ${b}")
        }
      case a :: b :: c :: Nil =>
        if ((a & 0xf0) == 0xe0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80) {
          Right((a & 0xf) << 12 | (b & 0x3f) << 6 | c & 0x3f)
        } else {
          Left(s"Invalid UTF-8 byte encoding. For characters represented by 3 bytes, the first byte must be < 0xf0 and the second and third by must be < 0xc0: ${a} ${b} ${c}")
        }
      case a :: b :: c :: d :: Nil =>
        if ((a & 0xf8) == 0xf0 && (b & 0xc0) == 0x80 && (c & 0xc0) == 0x80 && (d & 0xc0) == 0x80) {
          Right((a & 0x7) << 18 | (b & 0x3f) << 12 | (c & 0x3f) << 6 | d & 0x3f)
        } else {
          Left(s"Invalid UTF-8 byte encoding. For characters represented by 4 bytes, the first byte must be < 0xf8 and the second and third by must be < 0xc0: ${a} ${b} ${c}")
        }
      case _ =>
        throw new AssertionError(s"Invalid byte sequence length for UTF-8: ${bytes.size}. This is a cats-uri bug.")
    }

    @tailrec
    def loop(head: Byte, tail: List[Byte]): Either[String, String] = {
      val characterLength: Int = utf8ByteLength(head)

      if (tail.size < (characterLength - 1)) {
        Left(s"Invalid UTF-8 byte sequence in percent encoded value. Expected at least ${characterLength} more bytes, but only had ${tail.size + 1}")
      } else {
        tail.splitAt(characterLength - 1) match {
          case (chars, next) =>
            decodeToCodePoint(head :: chars) match {
              case Right(codePoint) =>
                builder.appendCodePoint(codePoint)
                next match {
                  case Nil =>
                    Right(builder.toString)
                  case head :: next =>
                    loop(head, next)
                }
              case Left(e) =>
                Left(e)
            }
        }
      }
    }

    loop(nel.head, nel.tail)
  }

  private val percentByteSubStringParser: Parser[String] =
    percentByteParser.rep.flatMap((nel: NonEmptyList[Byte]) =>
      utfBytesToString(nel).fold(
        e => Parser.failWith(e),
        Parser.pure
      )
    )

  /**
   * Percent decode a given `String`.
   */
  def decode(value: String): Either[String, String] =
    (percentByteSubStringParser | (Parser.anyChar *> Parser.charsWhile0(_ != '%')).string).repAs0[String].parseAll(value).fold(
      e => Left(s"Decoding the percent encoded value failed. This likely means there was an invalid percent encoded byte sequence or you have a literal '%' in the String which does not map to a percent encoded UTF-8 byte sequence"),
      value => Right(value)
    )

  def unsafeDecode(value: String): String =
    decode(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
