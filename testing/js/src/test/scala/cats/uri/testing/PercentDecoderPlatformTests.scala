package cats.uri.testing

import cats._
import cats.syntax.all._
import cats.uri._
import org.scalacheck._
import org.scalacheck.Prop._
import scala.collection.immutable.SortedSet
import munit._
import scala.scalajs.js.URIUtils

private[testing] abstract class PercentDecoderPlatformTests extends ScalaCheckSuite {
  property("PercentDecoder.decode should agree with decodeURIComponent"){
    forAll{(str: String) =>
      val encoded: String = PercentEncoder.encodeAll(str)
      val decoded: Either[String, String] = PercentDecoder.decode(encoded)
      val jsDecoded: Either[String, String] = ApplicativeError[Either[Throwable, *], Throwable].catchNonFatal(
        URIUtils.decodeURIComponent(encoded)
      ).leftMap(_.getLocalizedMessage)
      (decoded ?= jsDecoded) && (decoded ?= Right(str))
    }
  }
}

private[testing] object PercentDecoderPlatformTests
