/*
 * Copyright 2022 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package cats.uri

import cats._
import cats.syntax.all._
import cats.parse._
import org.typelevel.ci._
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import cats.uri.parsers._

/**
 * The scheme of a URI.
 *
 * Schemes are case-insensitive, but are canonically shown as lower case.
 *
 * {{{
 * scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
 * }}}
 *
 * @see
 *   [[https://datatracker.ietf.org/doc/html/rfc3986#section-3.1]]
 */
sealed trait Scheme extends Product with Serializable {
  def value: CIString

  // final //

  /** Renders a debug representation of this type.
    *
    * The differnece between this an `toString` is that this will render the
    * type as one might expect from a canonical case class.
    *
    * {{{
    * scala> Scheme.Http.toString
    * val res0: String = http
    *
    * scala> Scheme.Http.debugString
    * val res1: String = Scheme(value = http)
    * }}}
    */
  final def debugString: String = s"Scheme(value = ${toString})"

  override final def toString: String = value.toString.toLowerCase
}

object Scheme {
  private[this] final case class SchemeImpl(override val value: CIString) extends Scheme

  private[this] object SchemeImpl {
    def from(value: CIString): Scheme =
      ianaSchemeMapping.get(value).getOrElse(SchemeImpl.apply(value))

    def from(value: String): Scheme =
      from(CIString(value))
  }

  implicit val hashAndOrderForScheme: Hash[Scheme] with Order[Scheme] =
    new Hash[Scheme] with Order[Scheme] {
      override def hash(x: Scheme): Int = x.hashCode

      override def compare(x: Scheme, y: Scheme): Int =
        x.value.compare(y.value)
    }

  implicit val ordering: Ordering[Scheme] =
    hashAndOrderForScheme.toOrdering

  implicit val showForScheme: Show[Scheme] =
    Show.fromToString

  def unapply(value: Scheme): Some[CIString] =
    Some(value.value)

  /**
   * Parser for [[Scheme]].
   *
   * {{{
   * scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
   * }}}
   *
   * @see
   *   [[https://datatracker.ietf.org/doc/html/rfc3986#section-3.1]]
   */
  def parser: Parser[Scheme] =
    Rfc3986.schemeStr
      .map(value => SchemeImpl.from(CIString(value)))

  /**
   * A static mapping of known IANA schemes so that we can intern common schemes and skip
   * parsing ins some cases.
   */
  private val ianaSchemeMapping: SortedMap[CIString, Scheme] =
    SchemeDB.ianaSchemes.foldLeft(SortedMap.empty[CIString, Scheme]) {
      case (acc, value) =>
        // We go through the schemeStr here as a fail fast sanity check. We
        // have to be careful to bypass SchemeImpl.from, which would attempt
        // to intern the result, which would cause a loop.
        Rfc3986.schemeStr.parseAll(value.toString).fold(
          _ => throw new AssertionError(s"Static IANA scheme ${value} failed parsing. This is a cats-uri bug."),
          _ => acc + (value -> SchemeImpl(value))
        )
    }

  /**
   * A canonical set of schemes as registered with IANA.
   *
   * @see
   *   [[https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml]]
   */
  val ianaSchemes: SortedSet[Scheme] =
    ianaSchemeMapping.foldMap(value => SortedSet(value))

  def Http: Scheme =
    SchemeImpl.from("http")

  def Https: Scheme =
    SchemeImpl.from("https")

  def File: Scheme =
    SchemeImpl.from("file")

  /**
   * Attempt to create a [[Scheme]] from a `String`.
   */
  def fromString(value: String): Either[String, Scheme] = {
    val trimmed: String = value.trim
    ianaSchemeMapping
      .get(CIString(trimmed))
      .map(value => Right(value))
      .getOrElse(
        parser
          .parseAll(trimmed)
          .leftMap(_ =>
            s"Invalid URI scheme: ${value}. A URI Scheme must be at least one alpha character, followed by zero or more [A-Za-z0-9+-.] characters.")
      )
  }

  /**
   * Create a [[Scheme]] from a `String`, throwing an error if the `String` is not a valid URI
   * [[Scheme]].
   *
   * @note
   *   In general, it is recommended that you not use this method outside of test code or the
   *   REPL.
   */
  def unsafeFromString(value: String): Scheme =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
