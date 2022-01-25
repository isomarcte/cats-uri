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

import cats.uri._
import org.typelevel.literally.Literally

private[syntax] trait PasswordSyntax {
  implicit class PasswordContext(val sc: StringContext) {
    def password(args: Any*): Password = macro PasswordSyntax.make
  }
}

private object PasswordSyntax extends Literally[Password] {
  def validate(c: Context)(s: String) = {
    import c.universe._

    Password.fromString(s).map(_ => c.Expr(q"Password.unsafeFromString($s)"))
  }

  def make(c: Context)(args: c.Expr[Any]*): c.Expr[Password] =
    apply(c)(args: _*)
}
