package cats.uri.syntax

import cats.uri._
import scala.language.future
import scala.quoted.*

private[syntax] trait UserSyntax {
  extension (inline ctx: StringContext) {
    inline def user(inline args: Any*): User =
      UserSyntax.literal(ctx, args)
  }
}

private object UserSyntax {

  private def userExpr(sc: Expr[StringContext], args: Expr[Seq[Any]])(using q: Quotes): Expr[User] =
    sc.value match {
      case Some(sc) if sc.parts.size == 1 =>
        val value: String = sc.parts.head
        User.fromString(value).fold(
          e => {
            quotes.reflect.report.error(e)
            ???
          },
          _ => '{User.unsafeFromString(${Expr(value)})}
        )
      case Some(_) =>
        quotes.reflect.report.error("StringContext must be a single string literal")
        ???
      case None =>
        quotes.reflect.report.error("StringContext args must be statically known")
        ???
    }

  inline def literal(inline sc: StringContext, inline args: Any*): User =
    ${userExpr('sc, 'args)}
}
