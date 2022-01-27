package cats.uri.benchmarks

import org.scalacheck._
import cats.uri._
import org.openjdk.jmh.annotations._
import com.google.common.net.PercentEscaper
import scala.collection.immutable.BitSet

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
class PercentEncodingBenchmark {

  var currentSeed: Long = 0

  val genString: Gen[String] =
    Arbitrary.arbitrary[String]

  val guavaAlwaysEncodedCodepoints: BitSet =
    (('a' to 'z') ++ ('0' to '9')).foldLeft(BitSet.empty){
      case (acc, value) =>
        (acc + value.toInt) + value.toUpper.toInt
    }

  val guavaPercentEscaper: PercentEscaper =
    new PercentEscaper("", false)

  def generateString: String = {
    val s: String = genString(Gen.Parameters.default, rng.Seed(currentSeed)).getOrElse(throw new AssertionError("Failed to generate string"))
    currentSeed += 1L
    s
  }

  @Benchmark
  def catsUriPercentEncoder: String = {
    PercentEncoder.encode(guavaAlwaysEncodedCodepoints.contains)(generateString)
  }

  @Benchmark
  def guavaUriPercentEncoder: String = {
    guavaPercentEscaper.escape(generateString)
  }

  @Benchmark
  def http4sUriEncoder: String = {
    import org.http4s.Uri

    Uri.encode(generateString, java.nio.charset.StandardCharsets.UTF_8, false, c => guavaAlwaysEncodedCodepoints.contains(c.toInt))
  }
}
