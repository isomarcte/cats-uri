package cats.uri

import scala.util.control.NoStackTrace

  sealed trait DecodingError extends RuntimeException with NoStackTrace with Product with Serializable {

    /** The position in the input at which the error occurred.
      */
    def position: Option[Int]

    /**
      * A description of the error which is always safe to render, e.g. print
      * to the console, log out, etc.
      */
    def sanitizedMessage: String

    /** A detailed error message which may include specific parts of the input
      * string and thus, depending on the context, may not be safe to render.
      */
    def detailedMessage: Option[String]

    override final def getMessage: String = sanitizedMessage

    override final def toString: String =
      s"DecodingError(position = ${position}, sanitizedMessage = ${sanitizedMessage})"
  }

  object DecodingError {
    private[this] final case class DecodingErrorImpl(override val position: Option[Int], override val sanitizedMessage: String, override val detailedMessage: Option[String], cause: Option[Throwable]) extends DecodingError {
      override def getCause(): Throwable =
        cause.getOrElse(null)
    }

    def apply(
      position: Option[Int],
      sanitizedMessage: String,
      detailedMessage: Option[String],
      cause: Option[Throwable]
    ): DecodingError =
      DecodingErrorImpl(position, sanitizedMessage, detailedMessage, cause)

    def apply(
      position: Int,
      sanitizedMessage: String,
      detailedMessage: Option[String]
    ): DecodingError =
      apply(Some(position), sanitizedMessage, detailedMessage, None)

    def apply(
      position: Int,
      sanitizedMessage: String,
      detailedMessage: String
    ): DecodingError =
      apply(position, sanitizedMessage, Some(detailedMessage))

    def apply(
      position: Int,
      sanitizedMessage: String
    ): DecodingError =
      apply(position, sanitizedMessage, None)

    def apply(
      sanitizedMessage: String,
      detailedMessage: String
    ): DecodingError =
      apply(None, sanitizedMessage, Some(detailedMessage), None)
  }
