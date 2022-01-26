package cats.uri.testing

import cats.uri._
import org.scalacheck._
import org.scalacheck.Prop._
import scala.collection.immutable.SortedSet
import com.google.common.net.PercentEscaper
import munit._

private[testing] abstract class PercentEncoderPlatformTests extends ScalaCheckSuite
private[testing] object PercentEncoderPlatformTests
