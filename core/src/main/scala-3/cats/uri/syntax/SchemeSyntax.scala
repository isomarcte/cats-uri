package cats.uri.syntax

import cats.uri._
import scala.language.future
import scala.quoted.*
import org.typelevel.literally.Literally

private[syntax] trait SchemeSyntax {
  extension (inline ctx: StringContext) {
    inline def scheme(inline args: Any*): Scheme =
      SchemeSyntax.literal(ctx, args)
  }
}

private object SchemeSyntax {

  private def schemeExpr(sc: Expr[StringContext], args: Expr[Seq[Any]])(using q: Quotes): Expr[Scheme] =
    sc.value match {
      case Some(sc) if sc.parts.size == 1 =>
        val value: String = sc.parts.head
        Scheme.fromString(value).fold(
          e => {
            quotes.reflect.report.error(e)
            ???
          },
          _ => '{Scheme.unsafeFromString(${Expr(value)})}
        )
      case Some(_) =>
        quotes.reflect.report.error("StringContext must be a single string literal")
        ???
      case None =>
        quotes.reflect.report.error("StringContext args must be statically known")
        ???
    }

  inline def literal(inline sc: StringContext, inline args: Any*): Scheme =
    ${schemeExpr('sc, 'args)}
}
