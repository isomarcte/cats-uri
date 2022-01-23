package cats.uri.testing.parsers

import org.scalacheck.Prop._
import munit._
import cats.uri.scalacheck.parsers._
import cats.uri.parsers._

final class Rfc3986Tests extends ScalaCheckSuite {

  property("scheme strings should parse") {
    forAll(Rfc3986ScalacheckInstances.genSchemeString){(str: String) =>
      Rfc3986.schemeParser.parseAll(str) ?= Right(str)
    }
  }

  property("unreserved characters should parse") {
    forAll(Rfc3986ScalacheckInstances.genUnreservedChar){(char: Char) =>
      Rfc3986.unreservedChar.parseAll(char.toString) ?= Right(char)
    }
  }

  property("unreserved character strings should parse") {
    forAll(Rfc3986ScalacheckInstances.genUnreservedString){(str: String) =>
      Rfc3986.unreservedChar.rep0.string.parseAll(str) ?= Right(str)
    }
  }
}
