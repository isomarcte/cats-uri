package cats.uri.scalacheck

import cats.syntax.all._
import org.scalacheck._

private[scalacheck] object NonEmptyStringGen {
  val genNonEmptyString: Gen[String] =
    Arbitrary.arbitrary[String].flatMap(value =>
      if (value.length === 0) {
        Arbitrary.arbitrary[Char].map(_.toString)
      } else {
        Gen.const(value)
      }
    )
}
