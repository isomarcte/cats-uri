package cats.uri

import cats.syntax.all._
import cats.data._
import java.nio.charset.Charset
import cats.parse._
import cats.uri.parsers._

object PercentCodec {

  private def hexCharToInt(c: Char): Int =
    c match {
      case c if c >= '0' && c <= '9' => (c - '0')
      case c if c >= 'A' && c <= 'F' => 10 + (c - 'A')
      case c if c >= 'a' && c <= 'f' => 10 + (c - 'a')
    }

  private val percentByteParser: Parser[Byte] =
    Rfc3986.percentEncoded.map{
      case (a, b) =>
        ((hexCharToInt(a) << 4) | hexCharToInt(b)).toByte
    }

  private def percentByteSubStringParser(charset: Charset): Parser[String] =
    percentByteParser.rep.flatMap((nel: NonEmptyList[Byte]) =>
      try {
        Parser.pure(new String(nel.toList.toArray, charset))
      } catch {
        case e: Exception =>
          Parser.failWith(e.getLocalizedMessage)
      }
    )

  def decodeWithCharset(value: String, charset: Charset): Either[String, String] =
    (percentByteSubStringParser(charset) | Parser.charsWhile(_ != '%')).repAs0[String].parseAll(value).leftMap(_ =>
      s"Error encountered when attempting to percent decode a given String. This likely means that a given percent encoded sequence of bytes was invalid under the given character set ($charset)."
    )
}
