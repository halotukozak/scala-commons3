package com.avsystem.commons
package jiop

import java.util.IntSummaryStatistics
import scala.collection.Factory

opaque type ScalaJIntStream = JIntStream

object ScalaJIntStream {
  def apply(jStream: JIntStream): ScalaJIntStream = jStream

  extension (jStream: ScalaJIntStream) {
    def asJava: JIntStream = ScalaJIntStream(jStream)

    def close(): Unit =
      jStream.close()

    def isParallel: Boolean =
      jStream.isParallel

    def iterator: Iterator[Int] =
      jStream.iterator().asInstanceOf[JIterator[Int]].asScala

    inline def onClose(inline closeHandler: => Any): ScalaJIntStream =
      ScalaJIntStream(jStream.onClose(jRunnable(closeHandler)))

    def parallel: ScalaJIntStream =
      ScalaJIntStream(jStream.parallel())

    def sequential: ScalaJIntStream =
      ScalaJIntStream(jStream.sequential())

    def unordered: ScalaJIntStream =
      ScalaJIntStream(jStream.unordered())

    inline def allMatch(inline predicate: Int => Boolean): Boolean =
      jStream.allMatch(jIntPredicate(predicate))

    inline def anyMatch(inline predicate: Int => Boolean): Boolean =
      jStream.anyMatch(jIntPredicate(predicate))

    def asDoubleStream: ScalaJDoubleStream =
      ScalaJDoubleStream(jStream.asDoubleStream())

    def asLongStream: ScalaJLongStream =
      ScalaJLongStream(jStream.asLongStream())

    def average: Option[Double] =
      jStream.average.asScala

    def boxed: ScalaJStream[Int] =
      ScalaJStream(jStream.boxed.asInstanceOf[JStream[Int]])

    inline def collect[R](
      inline supplier: => R,
    )(
      inline accumulator: (R, Int) => Any,
      inline combiner: (R, R) => Any,
    ): R =
      jStream.collect(jSupplier(supplier), jObjIntConsumer(accumulator), jBiConsumer(combiner))

    def count: Long =
      jStream.count

    def distinct: ScalaJIntStream =
      ScalaJIntStream(jStream.distinct)

    inline def filter(inline predicate: Int => Boolean): ScalaJIntStream =
      ScalaJIntStream(jStream.filter(jIntPredicate(predicate)))

    def findAny: Option[Int] =
      jStream.findAny().asScala

    def findFirst: Option[Int] =
      jStream.findFirst.asScala

    inline def flatMap(mapper: Int => ScalaJIntStream): ScalaJIntStream =
      ScalaJIntStream(jStream.flatMap(jIntFunction(mapper)))

    inline def forEach(inline action: Int => Any): Unit =
      jStream.forEach(jIntConsumer(action))

    inline def forEachOrdered(inline action: Int => Any): Unit =
      jStream.forEachOrdered(jIntConsumer(action))

    def limit(maxSize: Long): ScalaJIntStream =
      ScalaJIntStream(jStream.limit(maxSize))

    inline def map(inline mapper: Int => Int): ScalaJIntStream =
      ScalaJIntStream(jStream.map(jIntUnaryOperator(mapper)))

    inline def mapToDouble(inline mapper: Int => Double): ScalaJDoubleStream =
      ScalaJDoubleStream(jStream.mapToDouble(jIntToDoubleFunction(mapper)))

    inline def mapToLong(inline mapper: Int => Long): ScalaJLongStream =
      ScalaJLongStream(jStream.mapToLong(jIntToLongFunction(mapper)))

    inline def mapToObj[U](inline mapper: Int => U): ScalaJStream[U] =
      ScalaJStream(jStream.mapToObj(jIntFunction(mapper)))

    def max: Option[Int] =
      jStream.max.asScala

    def min: Option[Int] =
      jStream.min.asScala

    inline def noneMatch(inline predicate: Int => Boolean): Boolean =
      jStream.noneMatch(jIntPredicate(predicate))

    inline def peek(inline action: Int => Any): ScalaJIntStream =
      ScalaJIntStream(jStream.peek(jIntConsumer(action)))

    inline def reduce(identity: Int)(inline op: (Int, Int) => Int): Int =
      jStream.reduce(identity, jIntBinaryOperator(op))

    inline def reduce(inline op: (Int, Int) => Int): Option[Int] =
      jStream.reduce(jIntBinaryOperator(op)).asScala

    def skip(n: Long): ScalaJIntStream =
      ScalaJIntStream(jStream.skip(n))

    def sorted: ScalaJIntStream =
      ScalaJIntStream(jStream.sorted)

    def sum: Int =
      jStream.sum

    def summaryStatistics: IntSummaryStatistics =
      jStream.summaryStatistics()

    def toArray: Array[Int] =
      jStream.toArray

    def to[C](using fac: Factory[Int, C]): C = {
      val b = fac.newBuilder
      forEachOrdered(b += _)
      b.result()
    }
  }
}
