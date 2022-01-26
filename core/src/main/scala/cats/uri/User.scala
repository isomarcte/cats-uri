package cats.uri

import cats._
import cats.syntax.all._
import scala.collection.immutable.SortedSet

/**
 * The user component of the userinfo in the authority of a URI.
 *
 * @note A [[User]] value is not permitted to be empty. If it could be empty
 *       then `Option[User]` and `User("")` would create ambiguity as they
 *       could both reasonably be represented by the userinfo string
 *       ":password".
 *
 * @see [[https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.1]]
 */
sealed abstract class User extends Product with Serializable {

  /**
   * The value of the user field.
   *
   * @note This value is ''not'' percent encoded and thus is ''not'' suitable
   *       for rendering in the construction of Uri values. You should use
   *       [[#encode]] to render this [[User]] as a percent encoded `String`.
   */
  def value: String

  /**
   *
   */
  final def encode: String =
    PercentEncoder.encode(User.userCodepoints.contains)(value)

  final def renderAsString: String =
    encode

  override final def toString: String = s"User(value = ${value})"
}

object User {

  private[this] final case class UserImpl(override val value: String) extends User

  private val userCodepoints: SortedSet[Int] =
    Constants.userChars.map(_.toInt)

  implicit val userPercentEncoder: PercentEncoder[User] =
    new PercentEncoder[User] {
      override def encode(a: User): String =
        a.encode

      override def addToAppender(a: User, appender: Renderable.Appender): Renderable.Appender =
        appender.appendString(a.encode)
    }

  implicit val hashAndOrderForUser: Hash[User] with Order[User] =
    new Hash[User] with Order[User] {
      override def hash(x: User): Int =
        x.hashCode

      override def compare(x: User, y: User): Int =
        x.value.compare(y.value)
    }

  implicit val ordering: Ordering[User] =
    hashAndOrderForUser.toOrdering

  implicit val showForUser: Show[User] =
    Show.fromToString

  def fromString(value: String): Either[String, User] =
    if (value.length > 0) {
      Right(UserImpl(value))
    } else {
      Left("User values can not be the empty string.")
    }

  def fromPercentEncodedString(value: String): Either[String, User] =
    PercentDecoder.decode(value).flatMap(fromString)

  def unsafeFromString(value: String): User =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def unsafeFromPercentEncodedString(value: String): User =
    fromPercentEncodedString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def unapply(value: User): Some[String] =
    Some(value.value)
}
