package cats.uri.testing

import cats.uri._
import munit._

final class UsersTests extends FunSuite {

  test("User should not permit empty user values"){
    assert(User.fromString("").isLeft)
  }
}
