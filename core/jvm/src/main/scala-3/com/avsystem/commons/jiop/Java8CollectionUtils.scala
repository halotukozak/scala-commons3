package com.avsystem.commons
package jiop

trait Java8CollectionUtils {
  extension [A](it: JIterator[A]) {
    inline def forEachRemaining(inline code: A => Any): Unit =
      it.forEachRemaining(jConsumer(code))
  }

  extension [A](it: JIterable[A]) {
    inline def forEach(inline code: A => Any): Unit =
      it.forEach(jConsumer(code))
  }

  extension [A](coll: JCollection[A]) {
    inline def removeIf(inline pred: A => Boolean): Unit =
      coll.removeIf(jPredicate(pred))

    def scalaStream: ScalaJStream[A] =
      coll.stream.asScala
  }

  extension [A](coll: JCollection[Int]) {
    def scalaIntStream: ScalaJIntStream =
      coll.stream.asScalaIntStream
  }

  extension [A](coll: JCollection[Long]) {
    def scalaLongStream: ScalaJLongStream =
      coll.stream.asScalaLongStream
  }

  extension [A](coll: JCollection[Double]) {
    def scalaDoubleStream: ScalaJDoubleStream =
      coll.stream.asScalaDoubleStream
  }

  extension [K, V](map: JMap[K, V]) {
    inline def compute(key: K, inline remappingFunction: (K, V) => V): V =
      map.compute(key, jBiFunction(remappingFunction))

    inline def computeIfAbsent(key: K)(inline mappingFunction: K => V): V =
      map.computeIfAbsent(key, jFunction(mappingFunction))

    inline def computeIfPresent(key: K)(inline remappingFunction: (K, V) => V): V =
      map.computeIfPresent(key, jBiFunction(remappingFunction))

    inline def forEach(inline action: (K, V) => Any): Unit =
      map.forEach(jBiConsumer(action))

    inline def merge(key: K, value: V)(inline remappingFunction: (V, V) => V): V =
      map.merge(key, value, jBiFunction(remappingFunction))

    inline def replaceAll(inline function: (K, V) => V): Unit =
      map.replaceAll(jBiFunction(function))
  }
}

object Java8CollectionUtils extends Java8CollectionUtils
