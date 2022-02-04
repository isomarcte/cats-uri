package cats.uri

import cats._
import cats.syntax.all._
import scala.collection.immutable.BitSet

final case class RegisteredName(value: String) extends AnyVal {
  def encode: String =
    PercentEncoder[RegisteredName].encode(this)

  def renderAsString: String =
    encode

  override final def toString: String =
    s"RegisteredName(value = ${value})" // Just for consistency with the other
                                        // types.
}

object RegisteredName {
  private[this] val registeredNameAllowedCodepoints: BitSet =
    Constants.registeredNameChars.foldMap(c => BitSet(c.toInt))

  implicit val registeredNamePercentCodec: PercentCodec[RegisteredName] =
    PercentCodec.from(
      PercentDecoder.fromDecodedString(value =>
        Right(RegisteredName(value))
      ),
      value => PercentEncoder.encode(registeredNameAllowedCodepoints.contains)(value.value)
    )

  implicit val registeredNameAppendable: Appendable[RegisteredName] =
    Appendable.fromRenderableString[RegisteredName]

  implicit val registeredNameHashAndOrder: Hash[RegisteredName] with Order[RegisteredName] =
    new Hash[RegisteredName] with Order[RegisteredName] {
      override def hash(x: RegisteredName): Int =
        x.hashCode

      override def compare(x: RegisteredName, y: RegisteredName): Int =
        x.value.compare(y.value)
    }

  implicit val registeredNameOrdering: Ordering[RegisteredName] =
    registeredNameHashAndOrder.toOrdering

  implicit val registeredNameShow: Show[RegisteredName] =
    Show.fromToString

  def fromPercentEncodedString(value: String): Either[DecodingError, RegisteredName] =
    PercentDecoder[RegisteredName].decode(value)

  def unsafeFromPercentEncodedString(value: String): RegisteredName =
    fromPercentEncodedString(value).fold(
      e => throw e,
      identity
    )
}
