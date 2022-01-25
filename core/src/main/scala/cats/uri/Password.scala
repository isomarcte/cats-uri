package cats.uri

import cats._
import cats.syntax.all._
import scala.collection.immutable.SortedSet

/**
 * The password component of the passwordinfo in the authority of a URI.
 *
 * @note A [[Password]] value is not permitted to be empty. If it could be empty
 *       then `Option[Password]` and `Password("")` would create ambiguity as they
 *       could both reasonably be represented by the passwordinfo string
 *       ":password".
 *
 * @see [[https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.1]]
 */
sealed trait Password extends Product with Serializable {
  def unsafeValue: String

  def render: String =
    PercentEncoder.encode(Password.passwordCodepoints.contains)(unsafeValue)

  override final def toString: String = "Password(<REDACTED>)"
}

object Password {

  private[this] final case class PasswordImpl(override val unsafeValue: String) extends Password

  private val passwordCodepoints: SortedSet[Int] =
    Constants.passwordChars.map(_.toInt)

  implicit val passwordPercentEncoder: PercentEncoder[Password] =
    new PercentEncoder[Password] {
      override def encode(a: Password): String =
        a.render
    }

  implicit val hashAndOrderForPassword: Hash[Password] with Order[Password] =
    new Hash[Password] with Order[Password] {
      override def hash(x: Password): Int =
        x.hashCode

      override def compare(x: Password, y: Password): Int =
        x.unsafeValue.compare(y.unsafeValue)
    }

  implicit val ordering: Ordering[Password] =
    hashAndOrderForPassword.toOrdering

  implicit val showForPassword: Show[Password] =
    Show.fromToString

  def fromString(value: String): Either[String, Password] =
    if (value.length > 0) {
      Right(PasswordImpl(value))
    } else {
      Left("Password values can not be the empty string.")
    }

  def unsafeFromString(value: String): Password =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def unapply(value: Password): Some[String] =
    Some(value.unsafeValue)
}
