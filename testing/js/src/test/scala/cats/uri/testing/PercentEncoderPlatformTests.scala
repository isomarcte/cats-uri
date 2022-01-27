package cats.uri.testing

import cats.uri._
import org.scalacheck._
import org.scalacheck.Prop._
import scala.collection.immutable.SortedSet
import munit._
import scala.scalajs.js.URIUtils

private[testing] abstract class PercentEncoderPlatformTests extends ScalaCheckSuite {
  import PercentEncoderPlatformTests._

  property("PercentEncoder.encode should agree with ECMAScript's encodeURIComponent"){
    forAll{(str: String) =>
      PercentEncoder.encode(c => defaultAllowedCharacterSet.contains(c))(str) ?= URIUtils.encodeURIComponent(str)
    }
  }
}

private[testing] object PercentEncoderPlatformTests {
  private def defaultAllowedCharacterSet: SortedSet[Int] =
    ((('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')).foldLeft(SortedSet.empty[Char]){
      case (acc, value) =>
        acc + value
    } + '%').map(_.toInt)
}
