package cats.uri.scalacheck.parsers

import org.scalacheck._

/**
 * Scalacheck generators for RFC 3986 grammar productions.
 */
object Rfc3986ScalacheckInstances {

  // Generators for grammar productions which are not modeled.

  val genUnreservedChar: Gen[Char] =
    Gen.oneOf(Gen.alphaChar, Gen.numChar, Gen.oneOf('-', '.', '_', '~'))

  val genUnreservedString: Gen[String] =
    Gen.listOf(genUnreservedChar).map(_.mkString)

  val genPercentEncodedString: Gen[String] =
    Gen.hexChar.flatMap(a => Gen.hexChar.map(b => s"%$a$b"))

  // Generators for grammar modeled productions

  val genSchemeString: Gen[String] =
    for {
      head <- Gen.alphaChar
      tail <- Gen.listOf(Gen.oneOf(Gen.alphaChar, Gen.numChar, Gen.const('+'), Gen.const('-'), Gen.const('.')))
    } yield s"${head}${tail.mkString}"
}
