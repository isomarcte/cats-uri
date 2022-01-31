package cats.uri.benchmarks

import org.scalacheck._
import cats.uri._
import org.openjdk.jmh.annotations._
import com.google.common.net.PercentEscaper
import scala.collection.immutable.BitSet

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
class PercentDecodingBenchmark {

  var currentSeed: Long = 0

  val genString: Gen[String] =
    Arbitrary.arbitrary[String]

  val genPred: Gen[Int => Boolean] =
    Arbitrary.arbitrary[Int => Boolean]

  def generateString: String = {
    val s: String = genString(Gen.Parameters.default, rng.Seed(currentSeed)).getOrElse(throw new AssertionError("Failed to generate string"))
    currentSeed += 1L
    s
  }

  def generateEncoderPredicate: Int => Boolean = {
    val f: Int => Boolean = genPred(Gen.Parameters.default, rng.Seed(currentSeed)).getOrElse(throw new AssertionError("Failed to generate encoder predicate"))
    currentSeed += 1L
    (codePoint: Int) => {
      (codePoint != '%'.toInt) && f(codePoint)
    }
  }

  def encodeString(value: String): String =
    PercentEncoder.encodeAll(value)

  def encodeMin(value: String): String =
    PercentEncoder.encodeMinimal(value)

  def encodeMixed(value: String, pred: Int => Boolean): String =
    PercentEncoder.encode(pred)(value)

  @Benchmark
  def catsUriPercentDecoder: String = {
    PercentDecoder.unsafeDecode(encodeString(generateString))
  }

  @Benchmark
  def catsUriPercentDecoder2: String = {
    PercentDecoder.decode2(encodeString(generateString)).fold(
      e => throw new AssertionError(e),
      identity
    )
  }

  @Benchmark
  def javaStandardLibPercentDecoder: String = {
    java.net.URLDecoder.decode(encodeString(generateString), "UTF-8")
  }

  @Benchmark
  def http4sUriDecoder: String = {
    import org.http4s.Uri
    Uri.decode(encodeString(generateString))
  }

  @Benchmark
  def catsUriPercentDecoderMin: String = {
    PercentDecoder.unsafeDecode(encodeMin(generateString))
  }

  @Benchmark
  def catsUriPercentDecoderMin2: String = {
    PercentDecoder.decode2(encodeMin(generateString)).fold(
      e => throw new AssertionError(e),
      identity
    )
  }

  @Benchmark
  def javaStandardLibPercentDecoderMin: String = {
    java.net.URLDecoder.decode(encodeMin(generateString), "UTF-8")
  }

  @Benchmark
  def http4sUriDecoderMin: String = {
    import org.http4s.Uri
    Uri.decode(encodeMin(generateString))
  }

  @Benchmark
  def catsUriPercentDecoderMixed: String = {
    PercentDecoder.unsafeDecode(encodeMixed(generateString, generateEncoderPredicate))
  }

  @Benchmark
  def catsUriPercentDecoderMixed2: String = {
    PercentDecoder.decode2(encodeMixed(generateString, generateEncoderPredicate)).fold(
      e => throw new AssertionError(e),
      identity
    )
  }

  @Benchmark
  def javaStandardLibPercentDecoderMixed: String = {
    java.net.URLDecoder.decode(encodeMixed(generateString, generateEncoderPredicate), "UTF-8")
  }

  @Benchmark
  def http4sUriDecoderMixed: String = {
    import org.http4s.Uri
    Uri.decode(encodeMixed(generateString, generateEncoderPredicate))
  }
}
