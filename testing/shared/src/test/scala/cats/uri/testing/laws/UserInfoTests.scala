package cats.uri.testing.laws

import cats.kernel.laws.discipline._
import munit._
import cats.uri.scalacheck.userinfo._
import cats.uri._

final class UserInfoTests extends DisciplineSuite {
  checkAll("Hash[UserInfo]", HashTests[UserInfo].hash)
  checkAll("Order[UserInfo]", OrderTests[UserInfo].order)
}
