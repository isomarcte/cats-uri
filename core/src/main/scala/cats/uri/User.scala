package cats.uri

sealed trait User extends Product with Serializable {
  def decodeValue: String
  def encodedValue: String
}

object User {

}
