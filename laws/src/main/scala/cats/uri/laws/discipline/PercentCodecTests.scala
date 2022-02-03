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

package cats.uri.laws.discipline

import cats.uri._
import cats.uri.laws._
import org.scalacheck.Prop._
import org.scalacheck._
import org.typelevel.discipline._

trait PercentCodecTests[A] extends Laws {
  def laws: PercentCodecLaws[A]

  def percentCodec(implicit A: Arbitrary[A]): RuleSet =
    new DefaultRuleSet(
      name = "percentCodec",
      parent = None,
      "percentCodec round trip" -> forAll((a: A) => laws.percentCodecRoundTrip(a))
    )
}

object PercentCodecTests {
  def apply[A](implicit D: PercentDecoder[A], E: PercentEncoder[A]): PercentCodecTests[A] =
    new PercentCodecTests[A] {
      override def laws: PercentCodecLaws[A] = PercentCodecLaws[A]
    }
}
