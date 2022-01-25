package cats.uri.testing

import org.scalacheck.Prop._
import munit._
import cats.uri._

final class PercentEncoderTests extends ScalaCheckSuite {
  property("Any String should able to be percent encoded"){
    forAll{(pred: Int => Boolean, str: String) =>
      val encoded: String = PercentEncoder.encode(pred)(str)
      val decoded: Either[String, String] = PercentDecoder.decode(encoded)
      (decoded ?= Right(str)) :| s"PercentEncoded $str, bytes (${str.getBytes.toList}): $encoded"
    }
  }
}
