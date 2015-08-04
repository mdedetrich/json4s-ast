package org.json4s.ast

sealed abstract class JValue extends Product with Serializable

case object JNull extends JValue

case class JString(value: String) extends JValue

object JNumber{
  private val mc = BigDecimal.defaultMathContext
  def apply(value: Int): JNumber = JNumber(BigDecimal(value))
  def apply(value: Byte): JNumber = JNumber(BigDecimal(value))
  def apply(value: Short): JNumber = JNumber(BigDecimal(value))
  def apply(value: Long): JNumber = JNumber(BigDecimal(value))
  def apply(value: BigInt): JNumber = JNumber(BigDecimal(value))
  def apply(value: Float): JNumber = JNumber({
    // BigDecimal.decimal doesn't exist on 2.10, so this is just the 2.11 implementation
    new BigDecimal(new java.math.BigDecimal(java.lang.Float.toString(value), mc), mc)
  })
  def apply(value: Double): JNumber = JNumber(BigDecimal(value))
}

case class JNumber(value: BigDecimal) extends JValue {
  def to[B](implicit bigDecimalConverter: JNumberConverter[B]) = bigDecimalConverter(value)
}

sealed abstract class JBoolean extends JValue {
  def isEmpty: Boolean
  def get: Boolean
}

object JBoolean {
  def apply(x: Boolean): JBoolean = if (x) JTrue else JFalse
  def unapply(x: JBoolean): Some[Boolean] = Some(x.isEmpty)
}

case object JTrue extends JBoolean {
  def isEmpty = false
  def get = true
}

case object JFalse extends JBoolean {
  def isEmpty = false
  def get = false
}

object JObject {

  /**
   * Create a [[JObject]] whose fields maintain insertion iteration order.
   *
   * The map that backs this JObject will have the same iteration order as the passed in vector.
   *
   * Some operations on the map will maintain the iteration order, these include the methods defined by
   * [[scala.collection.immutable.MapLike]], such as [[scala.collection.immutable.MapLike.+]] and
   * [[scala.collection.immutable.MapLike.++]]. Other operations will lose the iteration order, particularly the
   * methods defined by [[scala.collection.GenTraversable]], like [[scala.collection.GenTraversable.map]] and
   * [[scala.collection.GenTraversable.filter]].
   *
   * The performance characteristics of the map are amortized constant time for lookups and adding keys that didn't
   * previously exist, while updating values for existing keys and removing keys take linear time.
   */
  def ordered(value: Vector[(String,JValue)] = Vector.empty): JObject = JObject(new InsertionOrderMap(value))
}

case class JObject(value: Map[String,JValue] = Map.empty) extends JValue

object JArray {
  def apply(value: JValue, values: JValue*): JArray = JArray(value +: values.to[Vector])
}

case class JArray(value: Vector[JValue] = Vector.empty) extends JValue