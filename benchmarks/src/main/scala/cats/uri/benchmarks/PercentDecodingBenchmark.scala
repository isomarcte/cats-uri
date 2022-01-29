package cats.uri.benchmarks

import org.scalacheck._
import cats.uri._
import org.openjdk.jmh.annotations._
import com.google.common.net.PercentEscaper
import scala.collection.immutable.BitSet

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
class PercentDecodingBenchmark {

  var currentSeed: Long = 0

  val genString: Gen[String] =
    Arbitrary.arbitrary[String]

  def generateString: String = {
    val s: String = genString(Gen.Parameters.default, rng.Seed(currentSeed)).getOrElse(throw new AssertionError("Failed to generate string"))
    currentSeed += 1L
    s
  }

  def encodeString(value: String): String =
    PercentEncoder.encodeAll(value)

  def encodeMin(value: String): String =
    PercentEncoder.encodeMinimal(value)

  @Benchmark
  def catsUriPercentDecoder: Either[String, String] = {
    PercentDecoder.decode(encodeString(generateString))
  }

  @Benchmark
  def catsUriPercentDecoder3: Either[String, String] = {
    PercentDecoder.decode3(encodeString(generateString))
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
  def catsUriPercentDecoderMin: Either[String, String] = {
    PercentDecoder.decode(encodeMin(generateString))
  }

  @Benchmark
  def catsUriPercentDecoder3Min: Either[String, String] = {
    PercentDecoder.decode3(encodeMin(generateString))
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
}
