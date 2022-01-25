package cats.uri

trait PercentAllowedChars[A] extends Serializable {
  def allowedChar(c: Char): Boolean
}

object PercentAllowedChars {
  implicit def apply[A](implicit ev: PercentAllowedChars[A]): PercentAllowedChars[A] =
    ev
}
