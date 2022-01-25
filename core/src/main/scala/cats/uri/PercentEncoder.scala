package cats.uri

import cats.syntax.all._
import java.nio.CharBuffer
import java.nio.charset.CharsetEncoder
import java.lang.StringBuilder
import scala.annotation.tailrec
import java.nio.charset.StandardCharsets

trait PercentEncoder[A] extends Serializable { self =>
  def encode(a: A): String

  def contramap[B](f: B => A): PercentEncoder[B] =
    new PercentEncoder[B] {
      override def encode(b: B): String =
        self.encode(f(b))
    }
}

object PercentEncoder {
  implicit def apply[A](implicit ev: PercentEncoder[A]): PercentEncoder[A] =
    ev

  private def intToHexChar(i: Int): Char =
    i match {
      case i if i >= 0 && i <= 9 =>
        (i + '0'.toInt).toChar
      case i if i >= 10 && i <= 15 =>
        ('A' + i - 10).toChar
      case _ =>
        throw new AssertionError(s"Can not convert $i to a hex char. This is a cats-uri bug.")
    }

  def encode(allowedCodePointPredicate: Int => Boolean)(value: String): String = {
    val len: Int = value.length
    val acc: StringBuilder = new StringBuilder(value.length * 2)

    def appendByte(byte: Int): Unit = {
      acc.append('%')
      acc.append(intToHexChar(byte >> 4 & 0x0f))
      acc.append(intToHexChar(byte & 0x0f))
    }

    @tailrec
    def loop(index: Int): String =
      if (index >= len) {
        acc.toString()
      } else {
        val codePoint: Int = value.codePointAt(index)

        if (allowedCodePointPredicate(codePoint)) {
          acc.appendCodePoint(codePoint)
        } else if (codePoint < 0x80) {
          // 1 byte
          appendByte(codePoint)
        } else if (codePoint < 0x800) {
          // 2 bytes
          val byte1: Int = codePoint >> 6 | 0xc0
          val byte2: Int = (codePoint & 0x3f) | 0x80
          appendByte(byte1)
          appendByte(byte2)
        } else if (codePoint < 0x10000) {
          // 3 bytes
          val byte1: Int = codePoint >> 12 | 0xe0
          val byte2: Int = (codePoint >> 6 & 0x3f) | 0x80
          val byte3: Int = (codePoint & 0x3f) | 0x80
          appendByte(byte1)
          appendByte(byte2)
          appendByte(byte3)
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
        }
        loop(index + 1)
      }

    loop(0)
  }
}
