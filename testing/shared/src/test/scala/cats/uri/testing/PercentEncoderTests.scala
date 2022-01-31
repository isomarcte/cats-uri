package cats.uri.testing

import org.scalacheck.Prop._
import munit._
import cats.uri._

final class PercentEncoderTests extends PercentEncoderPlatformTests {
  property("Any String should able to be percent encoded"){
    forAll{(pred: Int => Boolean, str: String) =>
      val encoded: String = PercentEncoder.encode(pred)(str)
      val decoded: Either[DecodingError, String] = PercentDecoder.decode(encoded)
      (decoded ?= Right(str)) :| s"PercentEncoded $str, bytes (${str.getBytes.toList}): $encoded"
    }
  }

  test("% should always be encoded, even if the supplied predicate says it should not be."){
    assertEquals(PercentEncoder.encode(_ => true)("%"), "%25")
  }

  property("http4s"){
    import org.http4s.Uri
    forAll{(str: String) =>
      Uri.encode(str, java.nio.charset.StandardCharsets.UTF_8, false, _ => false) =? PercentEncoder.encodeAll(str)
    }
  }
}
