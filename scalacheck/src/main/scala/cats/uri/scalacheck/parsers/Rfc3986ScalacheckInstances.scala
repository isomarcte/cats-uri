package cats.uri.scalacheck.parsers

import org.scalacheck._

/**
 * Scalacheck generators for RFC 3986 grammar productions.
 */
object Rfc3986ScalacheckInstances {

  // Generators for grammar productions which are not modeled.

  val genUnreservedChar: Gen[Char] =
    Gen.oneOf(Gen.alphaChar, Gen.numChar, Gen.oneOf('-', '.', '_', '~'))

  /**
   * Generate an unreserved character, typed as a `String`. This is different
   * than `genUnreservedString`, which generates a `String` (of arbitrary
   * length) of unreserved characters. This generator always generate a
   * `String` of exactly one character from the unreserved set.
   */
  val genUnreservedCharString: Gen[String] =
    genUnreservedChar.map(_.toString)

  val genUnreservedString: Gen[String] =
    Gen.stringOf(genUnreservedChar)

  val genPercentEncodedString: Gen[String] =
    Gen.hexChar.flatMap(a => Gen.hexChar.map(b => s"%$a$b"))

  val genSubDelimChar: Gen[Char] =
    Gen.oneOf(
      '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '='
    )

  /**
   * Generate a sub-delim character, typed as a `String`. This is different
   * than `genSubDelimString`, which generates a `String` (of arbitrary
   * length) of sub-delim characters. This generator always generate a
   * `String` of exactly one character from the sub-delim set.
   */
  val genSubDelimCharString: Gen[String] =
    genSubDelimChar.map(_.toString)

  val genSubDelimString: Gen[String] =
    Gen.stringOf(genSubDelimChar)

  // Generators for grammar modeled productions

  /**
   * Generates string which is a valid user from the userinfo section of the
   * authority of a URI.
   */
  val genUserinfoUserString: Gen[String] =
    Gen.frequency(
      1 -> Gen.const(""),
      19 -> Gen.listOf(
        Gen.oneOf(
          genUnreservedCharString,
          genPercentEncodedString,
          genSubDelimCharString
        )
      ).map(_.mkString)
    )

  val genSchemeString: Gen[String] =
    for {
      head <- Gen.alphaChar
      tail <- Gen.listOf(Gen.oneOf(Gen.alphaChar, Gen.numChar, Gen.const('+'), Gen.const('-'), Gen.const('.')))
    } yield s"${head}${tail.mkString}"
}
