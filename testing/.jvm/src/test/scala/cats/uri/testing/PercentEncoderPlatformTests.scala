package cats.uri.testing

import scala.collection.immutable.SortedSet
import com.google.guava.net.PercentEscaper
import munit._

abstract class PercentEncoderPlatformTests extends ScalaCheckSuite {
  import PercentEncoderPlatformTests._

  property("PercentEncoder.encode should agree with Guava's PercentEscaper"){
    forAll{(str: String) =>
      PercentEncoder.encode(c => guavaCharacterSetCodePoints.contains(c))(str) =? minimalGuavaEncoder.encode(str)
    }
  }
}

object PercentEncoderPlatformTests {
  private def guavaCharacterSet: SortedSet[Char] =
    (('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')).foldLeft(SortedSet.empty[Char]){
      case (acc, value) =>
        acc + value
    } + '%'

  private val guavaCharacterSetCodePoints: SortedSet[Int] =
    guavaCharacterSetCodePoints.map(_.toInt)

  private val minimalGuavaEncoder: PercentEscaper =
    new PercentEscaper(guavaCharacterSet.mkString)
}
