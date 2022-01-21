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

package cats.uri.scalacheck

import cats.uri._
import org.scalacheck._

/** Scalacheck instances for [[Scheme]].
  */
private[scalacheck] trait SchemeScalacheckInstances {

  /** Generates canonical IANA schemes.
    */
  final val genCanonicalScheme: Gen[Scheme] =
    Gen.oneOf(Scheme.ianaSchemes)

  /** Generates valid, but likely non-canonical, schemes. Anything which claims
    * to properly handle the full domain of URIs should be able to handle
    * anything generated by this.
    */
  final val genScheme: Gen[Scheme] =
    for {
      head <- Gen.alphaChar
      tail <- Gen.listOf(Gen.oneOf(Gen.alphaChar, Gen.numChar, Gen.const('+'), Gen.const('-'), Gen.const('.')))
    } yield Scheme.unsafeFromString(s"${head}${tail.mkString}")

  final implicit val arbScheme: Arbitrary[Scheme] =
    Arbitrary(
      Gen.frequency(
        95 -> genCanonicalScheme,
        5 -> genScheme
      )
    )

  final implicit val cogenScheme: Cogen[Scheme] =
    Cogen[String].contramap(_.value.toString.toLowerCase)
}
