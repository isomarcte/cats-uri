package cats.uri.testing

import org.scalacheck._
import org.scalacheck.Prop._
import munit._
import cats.uri._
import scala.collection.immutable.BitSet
import java.nio.charset.StandardCharsets

final class PercentDecoderTests extends PercentDecoderPlatformTests {
  import UnicodeGenerators._

  test("PercentDecoder.decode should decode the empty string"){
    assertEquals(PercentDecoder.decode(""), Right(""))
  }

  property("PercentDecoder.decode should reject strings contain non-hex percent sequences or partial percent sequences."){
    forAll(genNonHexPercentPrefixString){(str: String) =>
      Prop(PercentDecoder.decode(str).isLeft)
    }
  }

  property("PercentDecoder.decode should fail on invalid unicode byte sequences"){
    forAllNoShrink(genInvalidPercentEncodedString){(str: String) =>
      PercentDecoder.decode(str).fold(
        _ => Prop.passed,
        value => Prop.falsified :| s"String ${str} should not have decoded, but it decoded into ${value} (bytes ${value.getBytes(StandardCharsets.UTF_8).toList})"
      )
    }
  }

  property("PercentDecoder.decode should decode any String which has at least the '%' character encoded"){
    forAll{(str: String) =>
      PercentDecoder.decode(PercentEncoder.encodeMinimal(str)) ?= Right(str)
    }
  }

  property("PercentDecoder.decode should fail on overlong UTF-8 percent encoded byte strings"){
    forAllNoShrink(genOverlongUTF8Encodings){(str: String) =>
      PercentDecoder.decode(str).fold(
        _ => Prop.passed,
        decoded => Prop.falsified :| s"Overlong UTF-8 byte string ${str} should not encode to ${decoded}. It should fail."
      )
    }
  }
}
