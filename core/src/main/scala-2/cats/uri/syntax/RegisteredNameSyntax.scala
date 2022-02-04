package cats.uri.syntax

import cats.uri._
import org.typelevel.literally.Literally

private[syntax] trait RegisteredNameSyntax {
  implicit class RegisteredNameContext(val sc: StringContext) {
    def regNameEncoded(args: Any*): RegisteredName = macro RegisteredNameSyntax.regNameEncoded.make
  }
}

private object RegisteredNameSyntax {

  private object regNameEncoded extends Literally[RegisteredName] {
    def validate(c: Context)(s: String): Either[String, c.Expr[RegisteredName]] = {
      import c.universe._

      RegisteredName.fromPercentEncodedString(s) match {
        case Left(e) => Left(e.getLocalizedMessage)
        case _ =>
          Right(c.Expr(q"RegisteredName.unsafeFromPercentEncodedString($s)"))
      }
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[RegisteredName] =
      apply(c)(args: _*)
  }
}
