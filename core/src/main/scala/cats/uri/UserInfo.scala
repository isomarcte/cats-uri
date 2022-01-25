package cats.uri

sealed trait UserInfo extends Product with Serializable {
  def user: Option[User]

  def password: Option[Password]
}

object UserInfo {
  final case class UserOnly(userValue: User) extends UserInfo {
    override final def user: Option[User] = Some(userValue)
    override final val password: Option[Password] = None
  }

  final case class PasswordOnly(passwordValue: Password) extends UserInfo {
    override final val user: Option[User] = None
    override final def password: Option[Password] = Some(passwordValue)
  }

  sealed trait UserAndPassword extends UserInfo {
    def userValue: User
    def passwordValue: Password

    override final def user: Option[User] = Some(userValue)
    override final def password: Option[Password] = Some(passwordValue)

    override final def toString: String = s"UserAndPassword(user = ${userValue}, password = ${password})"
  }

  object UserAndPassword {
    private[this] final case class UserAndPasswordImpl(override val userValue: User, override val passwordValue: Password) extends UserAndPassword

    def apply(user: User, password: Password): UserAndPassword =
      UserAndPasswordImpl(user, password)

    def unapply(value: UserAndPassword): Some[(User, Password)] =
      Some(value.userValue -> value.passwordValue)
  }

  def apply(user: User): UserInfo =
    UserOnly(user)

  def apply(password: Password): UserInfo =
    PasswordOnly(password)

  def apply(user: User, password: Password): UserInfo =
    UserAndPassword(user, password)
}
