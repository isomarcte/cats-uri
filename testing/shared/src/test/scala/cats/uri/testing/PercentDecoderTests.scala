package cats.uri.testing

import org.scalacheck._
import org.scalacheck.Prop._
import munit._
import cats.uri._
import scala.collection.immutable.BitSet

final class PercentDecoderTests extends ScalaCheckSuite {
  test("PercentDecoder.decode should decode the empty string"){
    assertEquals(PercentDecoder.decode(""), Right(""))
  }

  property("PercentDecoder.decode should pass through non-hex byte percent values"){
    forAll(PercentDecoderTests.genNonHexPercentPrefixString){(str: String) =>
      PercentDecoder.decode(str) ?= Right(str)
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

  /**
   * Generates `String` values which are percent prefix, then either contain
   * one valid hex char, or no valid hex chars. `String` values of this form
   * should be passed through percent decoding unchanged.
   *
   * @note They will not be symmetric with respect to encoding. URI emitting
   *       applications should encode the `String` "%" as "%25". Thus, while
   *       decoding the full `String` "%A" should yield "%A", a standards
   *       conforming application ''encoding'' it should yield "%25A".
   *
   * @note Percent decoding is distinct from URI parsing. So in this context
   *       it is completely valid to have characters that would be illegal in
   *       some or all parts of a URI.
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
