package cats.bootstring

import cats._
import cats.syntax.all._

sealed abstract class TMax extends Product with Serializable {
  def value: Int

  override final def toString: String = s"TMax(value = ${value})"
}

object TMax {
  private[this] final case class TMaxImpl(override val value: Int) extends TMax

  val PunycodeTMax: TMax = TMax.unsafeFromInt(26)

  def fromInt(value: Int): Either[String, TMax] =
    if (value >= 0) {
      Right(TMaxImpl(value))
    } else {
      Left(s"According to RFC-3492 damp values must be >= 0.")
    }

  def unsafeFromInt(value: Int): TMax =
    fromInt(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )

  def fromString(value: String): Either[String, TMax] =
    ApplicativeError[Either[Throwable, *], Throwable].catchNonFatal(
      value.toInt
    ).leftMap(_.getLocalizedMessage).flatMap(fromInt)

  def unsafeFromString(value: String): TMax =
    fromString(value).fold(
      e => throw new IllegalArgumentException(e),
      identity
    )
}
