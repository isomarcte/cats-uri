package cats.uri

import cats._
import cats.syntax.all._
import cats.uri.parsers._

/**
 * The userinfo from the authority of a Uri.
 *
 * @note In order to avoid ambiguous representations, this userinfo model can
 *       not model the empty `String`. This makes `UserInfo` distinct from
 *       `Option[UserInfo]`.
 *
 * @note In the deprecated format `user:password` the first non-percent
 *       encoded ':' character is a delimiter between the username and the
 *       password. In case where the password is "" this will yield a userinfo
 *       value with a user, no password, ''with a colon delimiter''. This is a
 *       unique value from just having a username.
 *
 * {{{
 * scala> import cats.uri._, cats.uri.syntax.userinfo._
 *
 * scala> userInfoEncoded"myUserName:"
 * val res0: cats.uri.UserInfo = UserInfo(user = Some(User(value = myUserName)), password = None, hasColonDelimiter = true)
 *
 * scala> userInfoEncoded"myUserName"
 * val res1: cats.uri.UserInfo = UserInfo(user = Some(User(value = myUserName)), password = None, hasColonDelimiter = false)
 *
 * scala> res0 == res1
 * val res2: Boolean = false
 *
 * scala> res0.render
 * val res3: String = myUserName:
 *
 * scala> res1.render
 * val res4: String = myUserName
 * }}}
 *
 * @note If the [[Password]] is present, then `hasColonDelimiter` ''must'' be
 *       true.
 *
 * @see [[https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.1]]
 */
sealed abstract class UserInfo extends Product with Serializable {

  /**
   * The [[User]] defined in this [[UserInfo]].
   */
  def user: Option[User]

  /**
   * The [[Password]] defined in this [[UserInfo]].
   *
   * @note WARNING! This field is deprecated in RFC-3986. It is only modeled
   *       for interoperability purposes. If your application is generating
   *       URI values you should ''not'' use this.
   */
  def password: Option[Password]

  /**
   * From RFC-3986.
   *
   * {{{
   * the presence or absence of delimiters within a userinfo subcomponent is
   * usually significant to its interpretation.
   * }}}
   *
   * Thus the userinfo string "foo:" is not the same as "foo", which is why
   * this is modeled here.
   *
   * This ''must'' be true if user and password are both empty or if user is
   * empty and password is non-empty. It ''may'' be true if user is non-empty,
   * but it may also be false.
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

  /**
   * [[UserInfo]] with no username or password, but with a ':' character,
   * e.g. ":".
   */
  val OnlyColonDelimiter: UserInfo =
    UserInfoImpl(None, None, true)

  def apply(user: User, hasColonDelimiter: Boolean): UserInfo =
    UserInfoImpl(Some(user), None, hasColonDelimiter)

  def apply(user: User): UserInfo =
    apply(user, false)

  /**
   * Create [[UserInfo]] from only [[Password]].
   *
   * @note WARNING! Use of the password field is deprecated in RFC-3986. It is
   *       only modeled for interoperability purposes. If your application is
   *       generating URI values you should ''not'' use this.
   */
  def apply(password: Password): UserInfo =
    UserInfoImpl(None, Some(password), true)

  /**
   * Create [[UserInfo]] from a [[User]] and [[Password]].
   *
   * @note WARNING! Use of the password field is deprecated in RFC-3986. It is
   *       only modeled for interoperability purposes. If your application is
   *       generating URI values you should ''not'' use this.
   */
  def apply(user: User, password: Password): UserInfo =
    UserInfoImpl(Some(user), Some(password), true)

  /**
   * Attempt to parse a [[UserInfo]] value from a percent encoded `String`.
   *
   * @note Unlike some other data structures in cats-uri, there is no
   *       corresponding `fromString` constructor. This is because
   *       [[UserInfo]] can not be represented as an unencoded `String` in any
   *       canonical format. This is due to the fact that the ':' character in
   *       the [[User]] portion of the `String` would create ambiguity with
   *       the ':' used as a delimiter between the user and password
   *       fields. If you wish to create a [[UserInfo]] value from a [[User]]
   *       and/or [[Password]] value, you can simply do the following.
   *
   * {{{
   * scala> User.fromString("user").map(u => UserInfo(u))
   * val res0: Either[String, cats.uri.UserInfo] = Right(UserInfo(user = Some(User(value = user)), password = None, hasColonDelimiter = false))
   *
   * scala> UserInfo(user"user") // If you are using the literal syntax from cats.uri.syntax.all._
   * val res1: cats.uri.UserInfo = UserInfo(user = Some(User(value = user)), password = None, hasColonDelimiter = false)
   * }}}
   */
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

  /**
   * As [[#fromPercentEncodedString]], but will throw on invalid values.
   *
   * @note Use of this method is discouraged outside of testing and the REPL.
   */
  def unsafeFromPercentEncodedString(value: String): UserInfo =
    fromPercentEncodedString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
