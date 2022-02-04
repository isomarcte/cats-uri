/*
 * Copyright 2022 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.uri.syntax

import cats.uri.*
import scala.language.future
import scala.quoted.*
import scala.compiletime.*

private[syntax] trait RegisteredNameSyntax {
  extension (inline ctx: StringContext) {
    inline def regNameEncoded(inline args: Any*): RegisteredName =
      RegisteredNameSyntax.regNameEncodedLiteral(ctx, args)
  }
}

private object RegisteredNameSyntax {

  private def regNameEncodedExpr(sc: Expr[StringContext], args: Expr[Seq[Any]])(
      using q: Quotes): Expr[RegisteredName] =
    sc.value match {
      case Some(sc) if sc.parts.size == 1 =>
        val value: String = sc.parts.head
        RegisteredName
          .fromPercentEncodedString(value)
          .fold(
            e => {
              quotes.reflect.report.errorAndAbort(e.sanitizedMessage)
            },
            _ => '{ RegisteredName.unsafeFromPercentEncodedString(${ Expr(value) }) }
          )
      case Some(_) =>
        quotes.reflect.report.errorAndAbort("StringContext must be a single string literal")
      case None =>
        quotes.reflect.report.errorAndAbort("StringContext args must be statically known")
    }

  inline def regNameEncodedLiteral(inline sc: StringContext, inline args: Any*): RegisteredName =
    ${ regNameEncodedExpr('sc, 'args) }
}
