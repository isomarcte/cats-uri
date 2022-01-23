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
   *
   * @see [[https://datatracker.ietf.org/doc/html/rfc3986#section-2.3]]
   */
  val unreservedChar: Parser[Char] =
    Rfc5234.alpha | Rfc5234.digit | Parser.charIn('-', '.', '_', '~')

  /**
   * A percent encoded (also commonly refered to as URL encoded) value.
   *
   * @note Warning! It is common for characters in URIs to be percent encoded
   *       even though they are not required to be, ''however'' this does not
   *       mean that a percent encoded character is valid in ''any'' portion
   *       of the URI. For this reason, normalization of the URI must take
   *       place ''after'' parsing. This is why this parser yields the literal
   *       input `String` on success, and not a `Byte` or `Char`.
   *
   * {{{
   * pct-encoded   = "%" HEXDIG HEXDIG
   * }}}
   *
   * @see [[https://datatracker.ietf.org/doc/html/rfc3986#section-2.1]]
   * @see [[https://en.wikipedia.org/wiki/Percent-encoding]]
   */
  val percentEncodedStr: Parser[String] =
    ((Parser.char('%') *> Rfc5234.hexdig.rep(2, 2))).string

  /**
   * A parser for a sub delimiter character.
   *
   * {{{
   * sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
   *                 / "*" / "+" / "," / ";" / "="
   * }}}
   *
   * @see [[https://datatracker.ietf.org/doc/html/rfc3986#section-2.2]]
   */
  val subDelimsChar: Parser[Char] =
    Parser.charIn('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=')

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
  val schemeStr: Parser[String] =
    (Rfc5234.alpha *> (Rfc5234.alpha | Rfc5234.digit | Parser.charIn('+', '-', '.')).rep0).string
}
