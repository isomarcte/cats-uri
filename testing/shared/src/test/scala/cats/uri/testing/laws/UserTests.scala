package cats.uri.testing.laws

import cats.kernel.laws.discipline._
import munit._
import cats.uri.scalacheck.user._
import cats.uri._

final class UserTests extends DisciplineSuite {
  checkAll("Hash[User]", HashTests[User].hash)
  checkAll("Order[User]", OrderTests[User].order)
}
