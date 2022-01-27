package cats.uri.testing

import org.scalacheck._
import org.scalacheck.Prop._
import munit._
import cats.uri._
import scala.collection.immutable.BitSet

final class PercentDecoderTests extends PercentDecoderPlatformTests {
  test("PercentDecoder.decode should decode the empty string"){
    assertEquals(PercentDecoder.decode(""), Right(""))
  }

  property("PercentDecoder.decode should reject strings contain non-hex percent sequences or partial percent sequences."){
    forAll(PercentDecoderTests.genNonHexPercentPrefixString){(str: String) =>
      Prop(PercentDecoder.decode(str).isLeft)
    }
  }

  property("PercentDecoder.decode should fail on invalid unicode byte sequences"){
    forAllNoShrink(PercentDecoderTests.genInvalidPercentEncodedString){(str: String) =>
      Prop(PercentDecoder.decode(str).isLeft)
    }
  }

  property("PercentDecoder.decode should any String which has at least the '%' character encoded"){
    forAll{(str: String) =>
      PercentDecoder.decode(PercentEncoder.encodeMinimal(str)) ?= Right(str)
    }
  }
}

object PercentDecoderTests {
  private[this] val MaxUnicodeCodePoint: Int = 0x10ffff
  private[this] val ValidHexUnicodeCodePoints: BitSet =
    "0123456789abcdef0123456789ABCDEF".toList.foldLeft(BitSet.empty){
      case (acc, value) => acc + value.toInt
    }

  private[this] val genUnicodeCodePoints: Gen[Int] =
    Gen.choose(0, MaxUnicodeCodePoint)

  private[this] val genNonHexCodePointStrings: Gen[String] =
    genUnicodeCodePoints.filterNot(
      ValidHexUnicodeCodePoints.contains
    ).map(codePoint => Character.toString(codePoint))

  private[this] val genInvalidUnicodeByteSequence: Gen[List[Byte]] = {
    val genByte: Gen[Byte] =
      Arbitrary.arbitrary[Byte]

    val genInvalid1ByteSequences: Gen[Byte] =
      genByte.map(_ | 0x80).map(_.toByte)

    val genInvalidNonLeadingByte: Gen[Byte] =
      genByte.flatMap(b =>
        Gen.oneOf(
          b & 0x7f,
          b | 0xc0
        ).map(_.toByte)
      )

    val genInvalid2ByteSequences: Gen[(Byte, Byte)] = {
      val genValidByte1: Gen[Byte] =
        genByte.map(b =>
          ((b | 0xc0) & 0xdf).toByte
        )

      genValidByte1.flatMap(a => genInvalidNonLeadingByte.map(b => (a, b)))
    }

    val genInvalid3ByteSequences: Gen[(Byte, Byte, Byte)] = {
      val genValidByte1: Gen[Byte] =
        genByte.map(b =>
          ((b | 0xe0) & 0xef).toByte
        )

      Gen.oneOf(
        genValidByte1.flatMap(a => genInvalidNonLeadingByte.flatMap(b => genByte.map(c => (a, b, c)))),
        genValidByte1.flatMap(a => genInvalidNonLeadingByte.flatMap(b => genInvalidNonLeadingByte.map(c => (a, b, c)))),
        genValidByte1.flatMap(a => genByte.flatMap(b => genInvalidNonLeadingByte.map(c => (a, b, c))))
      )
    }

    val genInvalid4ByteSequences: Gen[(Byte, Byte, Byte, Byte)] = {
      val genValidByte1: Gen[Byte] =
        genByte.map(b =>
          ((b | 0xf0) & 0xf7).toByte
        )

      Gen.oneOf(
        genValidByte1.flatMap(a => genInvalidNonLeadingByte.flatMap(b => genByte.flatMap(c => genByte.map(d => (a, b, c, d))))),
        genValidByte1.flatMap(a => genInvalidNonLeadingByte.flatMap(b => genInvalidNonLeadingByte.flatMap(c => genByte.map(d => (a, b, c, d))))),
        genValidByte1.flatMap(a => genInvalidNonLeadingByte.flatMap(b => genInvalidNonLeadingByte.flatMap(c => genInvalidNonLeadingByte.map(d => (a, b, c, d))))),
        genValidByte1.flatMap(a => genByte.flatMap(b => genInvalidNonLeadingByte.flatMap(c => genInvalidNonLeadingByte.map(d => (a, b, c, d))))),
        genValidByte1.flatMap(a => genByte.flatMap(b => genInvalidNonLeadingByte.flatMap(c => genByte.map(d => (a, b, c, d))))),
        genValidByte1.flatMap(a => genByte.flatMap(b => genByte.flatMap(c => genInvalidNonLeadingByte.map(d => (a, b, c, d)))))
      )
    }

    Gen.oneOf(
      genInvalid1ByteSequences.map(b => List(b)),
      genInvalid2ByteSequences.map{ case (a, b) => List(a, b)},
      genInvalid3ByteSequences.map{ case (a, b, c) => List(a, b, c)},
      genInvalid4ByteSequences.map{ case (a, b, c, d) => List(a, b, c, d)}
    )
  }

  private def byteToPercentHexString(b: Byte): String = {
    val hi: Int = b >> 4 & 0x0f
    val low: Int = b & 0x0f

    def intToHexChar(i: Int): Char =
      if (i >= 0 && i <= 9) {
        (i + '0'.toInt).toChar
      } else {
        ('A'.toInt + i - 10).toChar
      }

    s"%${intToHexChar(hi)}${intToHexChar(low)}"
  }

  val genInvalidPercentEncodedString: Gen[String] =
    genInvalidUnicodeByteSequence.flatMap(bytes =>
      bytes.map(byteToPercentHexString).mkString.foldLeft(Gen.const("")){
        case (acc, value) =>
          for {
            lowerCase <- Arbitrary.arbitrary[Boolean]
            acc <- acc
          } yield {
            val newValue: Char = if (lowerCase) value.toLower else value.toUpper
            acc ++ newValue.toString
          }
      }
    )

  /**
   * Generates `String` values which are percent prefix, then either contain
   * one valid hex char, or no valid hex chars.
   */
  val genNonHexPercentPrefixString: Gen[String] = {
    val genHexThenNonHex: Gen[String] =
      for {
        c <- Gen.hexChar
        nonHexCodePoint <- genNonHexCodePointStrings
      } yield s"${c}${nonHexCodePoint}"

    val genNonHexThenHex: Gen[String] =
      for {
        nonHexCodePoint <- genNonHexCodePointStrings
        c <- Gen.hexChar
      } yield s"${nonHexCodePoint}${c}"

    val genNonHexThenNonHex: Gen[String] =
      for {
        nonHexCodePointA <- genNonHexCodePointStrings
        nonHexCodePointB <- genNonHexCodePointStrings
      } yield nonHexCodePointA ++ nonHexCodePointB

    Gen.oneOf(
      genHexThenNonHex,
      genNonHexThenHex,
      genNonHexThenNonHex,
      Gen.hexChar.map(_.toString)
    ).map(value => s"%${value}")
  }
}
