package cats.uri.syntax

import cats.uri._
import scala.language.future
import scala.quoted.*

private[syntax] trait PasswordSyntax {
  extension (inline ctx: StringContext) {
    inline def password(inline args: Any*): Password =
      PasswordSyntax.literal(ctx, args)
  }
}

private object PasswordSyntax {

  private def passwordExpr(sc: Expr[StringContext], args: Expr[Seq[Any]])(using q: Quotes): Expr[Password] =
    sc.value match {
      case Some(sc) if sc.parts.size == 1 =>
        val value: String = sc.parts.head
        Password.fromString(value).fold(
          e => {
            quotes.reflect.report.error(e)
            ???
          },
          _ => '{Password.unsafeFromString(${Expr(value)})}
        )
      case Some(_) =>
        quotes.reflect.report.error("StringContext must be a single string literal")
        ???
      case None =>
        quotes.reflect.report.error("StringContext args must be statically known")
        ???
    }

  inline def literal(inline sc: StringContext, inline args: Any*): Password =
    ${passwordExpr('sc, 'args)}
}
