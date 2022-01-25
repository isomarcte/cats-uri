package cats.uri

import cats._
import cats.syntax.all._
import cats.uri.parsers._

sealed trait UserInfo extends Product with Serializable {
  def user: Option[User]

  def password: Option[Password]

  /**
   * From RFC-3986.
   *
   * {{{
   * the presence or absence of delimiters within a userinfo subcomponent is
   usually significant to its interpretation.
   * }}}
   *
   * Thus the userinfo string "foo:" is not the same as "foo". This ''must''
   * be true if user and password are both empty or if user is empty and
   * password is non-empty. It ''may'' be true if user is non-empty, but it
   * may also be false.
   *
   * @see [[https://datatracker.ietf.org/doc/html/rfc3986#section-6.2.3]]
   */
  def hasColonDelimiter: Boolean

  def render: String = {
    val colonString: String =
      if (hasColonDelimiter) {
        ":"
      } else {
        ""
      }

    user.fold("")(_.render) ++ colonString ++ password.fold("")(_.render)
  }

  override final def toString: String = s"UserInfo(user = ${user}, password = ${password}, hasColonDelimiter = ${hasColonDelimiter})"
}

object UserInfo {
  private[this] final case class UserInfoImpl(override val user: Option[User], override val password: Option[Password], override val hasColonDelimiter: Boolean) extends UserInfo

  implicit val hashAndOrderForUserInfo: Hash[UserInfo] with Order[UserInfo] =
    new Hash[UserInfo] with Order[UserInfo] {
      override def hash(x: UserInfo): Int =
        x.hashCode

      override def compare(x: UserInfo, y: UserInfo): Int =
        x.user.compare(y.user) match {
          case 0 =>
            x.password.compare(y.password) match {
              case 0 =>
                x.hasColonDelimiter.compare(y.hasColonDelimiter)
              case otherwise =>
                otherwise
            }
          case otherwise =>
            otherwise
        }
    }

  implicit val ordering: Ordering[UserInfo] =
    hashAndOrderForUserInfo.toOrdering

  implicit val showForUserInfo: Show[UserInfo] =
    Show.fromToString

  implicit val userInfoPercentEncoder: PercentEncoder[UserInfo] =
    new PercentEncoder[UserInfo] {
      override def encode(a: UserInfo): String =
        a.render
    }

  val OnlyColonDelimiter: UserInfo =
    UserInfoImpl(None, None, true)

  def apply(user: User, hasColonDelimiter: Boolean): UserInfo =
    UserInfoImpl(Some(user), None, hasColonDelimiter)

  def apply(user: User): UserInfo =
    apply(user, false)

  def apply(password: Password): UserInfo =
    UserInfoImpl(None, Some(password), true)

  def apply(user: User, password: Password): UserInfo =
    UserInfoImpl(Some(user), Some(password), true)

  def fromPercentEncodedString(value: String): Either[String, UserInfo] =
    Rfc3986.userinfo.parseAll(value).leftMap(_ => "Invalid percent encoded UserInfo String.").flatMap{
      case (user, colon, password) if user.isDefined || colon.isDefined || password.isDefined =>
        for {
          u <- user.traverse(User.fromPercentEncodedString)
          p <- password.traverse(Password.fromPercentEncodedString)
        } yield UserInfoImpl(u, p, colon.isDefined)
      case _ =>
        Left("The empty string is not a valid UserInfo value.")
    }

  def unsafeFromPercentEncodedString(value: String): UserInfo =
    fromPercentEncodedString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
