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

  private val percentByteSubStringParser: Parser[String] =
    percentByteParser.rep.map((nel: NonEmptyList[Byte]) =>
      new String(nel.toList.toArray, StandardCharsets.UTF_8)
    )

  def decode(value: String): String =
    (percentByteSubStringParser | Parser.charsWhile(_ != '%')).repAs0[String].parseAll(value).fold(
      e => throw new AssertionError(s"Error encountered during percent decoding, but this should not be possible. This is a cats-uri bug."),
      identity
    )
}
