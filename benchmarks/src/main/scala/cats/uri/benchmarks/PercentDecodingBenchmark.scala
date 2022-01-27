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

  @Benchmark
  def catsUriPercentDecoder: Either[String, String] = {
    PercentDecoder.decode(encodeString(generateString))
  }

  @Benchmark
  def javaStandardLibPercentDecoder: String = {
    java.net.URLDecoder.decode(encodeString(generateString), java.nio.charset.StandardCharsets.UTF_8)
  }

  @Benchmark
  def http4sUriDecoder: String = {
    import org.http4s.Uri
    Uri.decode(encodeString(generateString))
  }
}
