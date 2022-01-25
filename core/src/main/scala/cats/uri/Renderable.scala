package cats.uri

trait Renderable[A] extends Serializable { self =>
  def renderAsString(a: A): String

  def contramap[B](f: B => A): Renderable[B] =
    new Renderable[B] {
      override def renderAsString(b: B): String =
        self.renderAsString(f(b))
    }
}

private[uri] trait RenderableLowPriorityInstances0 {
  implicit def renderableFromPercentEncoder[A](implicit ev: PercentEncoder[A]): Renderable[A] =
    new Renderable[A] {
      override def renderAsString(a: A): String =
        ev.encode(a)
    }
}

object Renderable extends RenderableLowPriorityInstances0 {
  implicit def apply[A](implicit ev: Renderable[A]): Renderable[A] =
    ev

  def instance[A](f: A => String): Renderable[A] =
    new Renderable[A] {
      override def renderAsString(a: A): String =
        f(a)
    }
}
