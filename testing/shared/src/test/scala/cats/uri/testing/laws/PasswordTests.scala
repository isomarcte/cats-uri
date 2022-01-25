package cats.uri.testing.laws

import cats.kernel.laws.discipline._
import munit._
import cats.uri.scalacheck.password._
import cats.uri._

final class PasswordTests extends DisciplineSuite {
  checkAll("Hash[Password]", HashTests[Password].hash)
  checkAll("Order[Password]", OrderTests[Password].order)
}
