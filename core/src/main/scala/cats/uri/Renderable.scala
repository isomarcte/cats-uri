package cats.uri

trait Renderable[A] extends Serializable { self =>
  def renderAsString(a: A): String
}

object Renderable {
  implicit def apply[A](implicit ev: Renderable[A]): Renderable[A] =
    ev

  def instance[A](f: A => String): Renderable[A] =
    new Renderable[A] {
      override def renderAsString(a: A): String =
        f(a)
    }
}
