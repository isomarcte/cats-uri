package cats.uri

import java.lang.StringBuilder

/**
 * A very light weight typeclass for rendering values as `String` values.
 *
 * "Rendering" in this context means converting the value into a `String`
 * which might be parsed as a valid Uri representation of the given
 * component. This often means percent encoding a value and can include some
 * forms of normalization.
 *
 * This is very similar to `Renderable` from http4s or `Accumulator` from
 * cats-parse, but it is specialized to `String` values with an emphasis on a
 * subset of the `StringBuilder` API.
 */
trait Renderable[A] extends Serializable { self =>
  import Renderable._

  /**
   * Render the given value as a `String`.
   */
  def renderAsString(a: A): String

  /**
   * Append this value to a [[Renderable#Appender]].
   */
  def addToAppender(a: A, appender: Appender): Appender
}

object Renderable {

  /**
   * A very small builder like class, specialized to `String`. It is
   * effectively a very basic wrapper over a `StringBuilder`.
   */
  sealed abstract class Appender {
    protected def stringBuilder: StringBuilder

    /**
     * Append the given unicode codepoint to the [[Appender]].
     */
    final def appendCodePoint(value: Int): Appender = {
      stringBuilder.appendCodePoint(value)
      this
    }

    /**
     * Append the given `String` to the [[Appender]].
     */
    final def appendString(value: String): Appender = {
      stringBuilder.append(value)
      this
    }

    /**
     * Append the given `Char` to the [[Appender]].
     */
    final def appendChar(value: Char): Appender = {
      stringBuilder.append(value)
      this
    }

    /**
     * Render the current value of the [[Appender]] to a `String`.
     */
    final def renderAsString: String =
      stringBuilder.toString
  }

  object Appender {

    /**
     * Create a new empty [[Appender]] value with the given size hint for the
     * pre-allocated buffer.
     */
    def instance(sizeHint: Int): Appender =
      new Appender {
        override val stringBuilder: StringBuilder = new StringBuilder(sizeHint)
      }

    /**
     * Create a new empty [[Appender]] value.
     *
     * @note If you have a notion of the size of the output, you might
     *       consider using the constructor which takes a size hint.
     */
    def instance: Appender =
      new Appender {
        override val stringBuilder: StringBuilder = new StringBuilder()
      }
  }
}
