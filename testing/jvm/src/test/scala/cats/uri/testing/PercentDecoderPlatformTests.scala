package cats.uri.testing

import cats.uri._
import munit._
import org.scalacheck.Prop._
import org.scalacheck._

private[testing] abstract class PercentDecoderPlatformTests extends ScalaCheckSuite {
  property("PercentDecoder.decode should agree with java.net.URLDecoder.decode"){
    forAll{(str: String) =>
      val encoded: String = PercentEncoder.encodeAll(str)
      val decoded: Either[String, String] = PercentDecoder.decode(encoded)
      val javaDecoded: String = java.net.URLDecoder.decode(encoded, "UTF-8")
      (decoded ?= Right(javaDecoded)) && (decoded ?= Right(str))
    }
  }
}

private[testing] object PercentDecoderPlatformTests
