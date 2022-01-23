package cats.uri.parsers

import cats.parse._

/**
 * Parsers for RFC-3986 grammar productions.
 *
 * @see [[https://datatracker.ietf.org/doc/html/rfc3986#appendix-A]]
 */
object Rfc3986 {

  // Parsers which do not emit modeled types

  /**
   * An unreserved character according to RFC-3986.
   *
   * @note unreserved is ''not'' the compliment of reserved.
   *
   * {{{
   * unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
   * }}}
   */
  val unreservedChar: Parser[Char] =
    Rfc5234.alpha | Rfc5234.digit | Parser.charIn('-', '.', '_', '~')

  // Parsers for modeled types

  /**
   * Parser for a `String` which reprsents a Scheme.
   *
   * {{{
   * scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
   * }}}
   *
   * @see
   *   [[https://datatracker.ietf.org/doc/html/rfc3986#section-3.1]]
   */
  val schemeParser: Parser[String] =
    (Rfc5234.alpha *> (Rfc5234.alpha | Rfc5234.digit | Parser.charIn('+', '-', '.')).rep0).string
}
