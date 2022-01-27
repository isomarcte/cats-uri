package cats.uri

import cats.syntax.all._
import java.nio.CharBuffer
import java.nio.charset.CharsetEncoder
import java.nio.charset.StandardCharsets
import java.lang.StringBuilder
import scala.annotation.tailrec

/**
 * A typeclass for percent encoding values.
 *
 * The exact set of characters which are required to be percent encoded is
 * dependent on the Uri component and the Uri scheme in play.
 *
 * @see [[https://datatracker.ietf.org/doc/html/rfc3986#section-2.1]]
 */
trait PercentEncoder[A] extends Renderable[A] { self =>

  /**
   * Encode the given value as a percent encoded `String`.
   */
  def encode(a: A): String

  override final def renderAsString(a: A): String =
    encode(a)
}

object PercentEncoder {
  implicit def apply[A](implicit ev: PercentEncoder[A]): PercentEncoder[A] =
    ev

  private val PercentUnicodeCodePoint: Int = 0x25

  private val HexChars: Array[Char] =
    "0123456789ABCDEF".toArray

  /**
   * Percent encode a `String` value.
   *
   * Percent encoding for URI values is always defined to use the UTF-8 byte
   * representation of encoded characters. For this reason, all `String`
   * values are safe to encode.
   *
   * The set of characters which need to be encoded is dependent on the
   * specific URI component being encoded as well as the scheme. This function
   * is a general utility function and end users should generally prefer
   * instances of the [[PercentEncoder]] class specific to some data
   * type. This function may be of use to library authors implementing rules
   * for a specific scheme, lacking native support in cats-uri.
   *
   * @note The unicode code point corresponding to '%' will ''always'' be
   *       encoded, even if the provided predicate returns `false` for this
   *       value. This is because there is no `String` which can be considered
   *       "percent encoded" if it contains unencoded '%' values.
   */
  def encode(allowedCodePointPredicate: Int => Boolean)(value: String): String = {
    val len: Int = value.length
    val acc: StringBuilder = new StringBuilder(len * 3)
    val buffer: CharBuffer = CharBuffer.allocate(12)

    def appendByte(byte: Int): Unit = {
      buffer.put('%')
      buffer.put(HexChars(byte >> 4 & 0x0f))
      buffer.put(HexChars(byte & 0x0f))
      // acc.append(HexChars(byte >> 4 & 0x0f))
      // acc.append(HexChars(byte & 0x0f))
    }

    @tailrec
    def loop(index: Int): String =
      if (index >= len) {
        acc.toString()
      } else {
        val codePoint: Int = value.codePointAt(index)
        val indexIncrement: Int = if (codePoint >= 0x10000) 2 else 1

        if (allowedCodePointPredicate(codePoint) && codePoint != PercentUnicodeCodePoint) {
          acc.appendCodePoint(codePoint)
        } else {
          buffer.clear
          if (codePoint < 0x80) {
            // 1 byte
            appendByte(codePoint)
            buffer.flip
            acc.append(buffer, 0, 3)
          } else if (codePoint < 0x800) {
            // 2 bytes
            val byte1: Int = codePoint >> 6 | 0xc0
            val byte2: Int = (codePoint & 0x3f) | 0x80
            appendByte(byte1)
            appendByte(byte2)
            buffer.flip
            acc.append(buffer, 0, 6)
          } else if (codePoint < 0x10000) {
            // 3 bytes
            val byte1: Int = codePoint >> 12 | 0xe0
            val byte2: Int = (codePoint >> 6 & 0x3f) | 0x80
            val byte3: Int = (codePoint & 0x3f) | 0x80
            appendByte(byte1)
            appendByte(byte2)
            appendByte(byte3)
            buffer.flip
            acc.append(buffer, 0, 9)
          } else {
            // 4 bytes
            val byte1: Int = codePoint >> 18 | 0xf0
            val byte2: Int = (codePoint >> 12 & 0x3f) | 0x80
            val byte3: Int = (codePoint >> 6 & 0x3f) | 0x80
            val byte4: Int = (codePoint & 0x3f) | 0x80
            appendByte(byte1)
            appendByte(byte2)
            appendByte(byte3)
            appendByte(byte4)
            buffer.flip
            acc.append(buffer, 0, 12)
          }
        }

        loop(index + indexIncrement)
      }

    loop(0)
  }

  /**
   * As [[#encode]], but encodes all characters in the `String`.
   *
   * @note Use of this method is usually wrong. RFC-3986 states that URI
   *       producing applications should only encode those character required
   *       by the component in question.
   */
  def encodeAll(value: String): String =
    encode(_ => false)(value)

  /**
   * As [[#encode]], but only encodes '%' in the `String`. This is the minimal
   * encoding which can be considered a valid percent encoded `String`.
   *
   * @note Use of this method is usually wrong. Most URI components require
   *       some additional characters to be percent encoded.
   */
  def encodeMinimal(value: String): String =
    encode(_ => true)(value)
}
