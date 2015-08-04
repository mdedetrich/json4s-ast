package org.json4s.ast

/**
 * An immutable map that maintains insertion order.
 *
 * It has the following performance characteristics:
 *
 * - Lookup: eC
 * - Add: eC, assuming the element isn't already in the map.
 * - Update: L
 * - Remove: L
 *
 * For some operations on this map, such as add, concatenate, remove, the insertion order of the new map will also be
 * maintained. For other operations, specifically those defined by GenMap, GenIterable, GenTraversable, etc, such as
 * map, filter, collect, the returned map will lose the insertion order.
 */
private class InsertionOrderMap[K, V] private (underlying: Map[K, V], values: Vector[(K, V)]) extends Map[K, V] {

  def this(values: Vector[(K, V)] = Vector.empty) = this(values.toMap, values)

  def +[B1 >: V](kv: (K, B1)) = {
    if (contains(kv._1)) {
      new InsertionOrderMap(underlying + kv, values.map {
        case (key, _) if key == kv._1 => kv
        case other => other
      })
    } else {
      new InsertionOrderMap(underlying + kv, values :+ kv)
    }
  }

  def get(key: K) = underlying.get(key)

  def iterator = values.iterator

  def -(key: K) = new InsertionOrderMap(underlying - key, values.filterNot(_._1 == key))
}