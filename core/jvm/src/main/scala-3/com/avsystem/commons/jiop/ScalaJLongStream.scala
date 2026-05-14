package com.avsystem.commons
package jiop

import java.util.LongSummaryStatistics
import scala.collection.Factory

opaque type ScalaJLongStream = JLongStream

object ScalaJLongStream {
  def apply(jStream: JLongStream): ScalaJLongStream = jStream

  extension (jStream: ScalaJLongStream) {
    def asJava: JLongStream = ScalaJLongStream(jStream)

    def close(): Unit =
      jStream.close()

    def isParallel: Boolean =
      jStream.isParallel

    def iterator: Iterator[Long] =
      jStream.iterator().asInstanceOf[JIterator[Long]].asScala

    inline def onClose(inline closeHandler: => Any): ScalaJLongStream =
      ScalaJLongStream(jStream.onClose(jRunnable(closeHandler)))

    def parallel: ScalaJLongStream =
      ScalaJLongStream(jStream.parallel())

    def sequential: ScalaJLongStream =
      ScalaJLongStream(jStream.sequential())

    def unordered: ScalaJLongStream =
      ScalaJLongStream(jStream.unordered())

    inline def allMatch(inline predicate: Long => Boolean): Boolean =
      jStream.allMatch(jLongPredicate(predicate))

    inline def anyMatch(inline predicate: Long => Boolean): Boolean =
      jStream.anyMatch(jLongPredicate(predicate))

    def asDoubleStream: ScalaJDoubleStream =
      ScalaJDoubleStream(jStream.asDoubleStream())

    def average: Option[Double] =
      jStream.average.asScala

    def boxed: ScalaJStream[Long] =
      ScalaJStream(jStream.boxed.asInstanceOf[JStream[Long]])

    inline def collect[R](
      inline supplier: => R,
    )(
      inline accumulator: (R, Long) => Any,
      inline combiner: (R, R) => Any,
    ): R =
      jStream.collect(jSupplier(supplier), jObjLongConsumer(accumulator), jBiConsumer(combiner))

    def count: Long =
      jStream.count

    def distinct: ScalaJLongStream =
      ScalaJLongStream(jStream.distinct)

    inline def filter(inline predicate: Long => Boolean): ScalaJLongStream =
      ScalaJLongStream(jStream.filter(jLongPredicate(predicate)))

    def findAny: Option[Long] =
      jStream.findAny().asScala

    def findFirst: Option[Long] =
      jStream.findFirst.asScala

    inline def flatMap(mapper: Long => ScalaJLongStream): ScalaJLongStream =
      ScalaJLongStream(jStream.flatMap(jLongFunction(mapper)))

    inline def forEach(inline action: Long => Any): Unit =
      jStream.forEach(jLongConsumer(action))

    inline def forEachOrdered(inline action: Long => Any): Unit =
      jStream.forEachOrdered(jLongConsumer(action))

    def limit(maxSize: Long): ScalaJLongStream =
      ScalaJLongStream(jStream.limit(maxSize))

    inline def map(inline mapper: Long => Long): ScalaJLongStream =
      ScalaJLongStream(jStream.map(jLongUnaryOperator(mapper)))

    inline def mapToDouble(inline mapper: Long => Double): ScalaJDoubleStream =
      ScalaJDoubleStream(jStream.mapToDouble(jLongToDoubleFunction(mapper)))

    inline def mapToInt(inline mapper: Long => Int): ScalaJIntStream =
      ScalaJIntStream(jStream.mapToInt(jLongToIntFunction(mapper)))

    inline def mapToObj[U](inline mapper: Long => U): ScalaJStream[U] =
      ScalaJStream(jStream.mapToObj(jLongFunction(mapper)))

    def max: Option[Long] =
      jStream.max.asScala

    def min: Option[Long] =
      jStream.min.asScala

    inline def noneMatch(inline predicate: Long => Boolean): Boolean =
      jStream.noneMatch(jLongPredicate(predicate))

    inline def peek(inline action: Long => Any): ScalaJLongStream =
      ScalaJLongStream(jStream.peek(jLongConsumer(action)))

    inline def reduce(identity: Long)(inline op: (Long, Long) => Long): Long =
      jStream.reduce(identity, jLongBinaryOperator(op))

    inline def reduce(inline op: (Long, Long) => Long): Option[Long] =
      jStream.reduce(jLongBinaryOperator(op)).asScala

    def skip(n: Long): ScalaJLongStream =
      ScalaJLongStream(jStream.skip(n))

    def sorted: ScalaJLongStream =
      ScalaJLongStream(jStream.sorted)

    def sum: Long =
      jStream.sum

    def summaryStatistics: LongSummaryStatistics =
      jStream.summaryStatistics()

    def toArray: Array[Long] =
      jStream.toArray

    def to[C](using fac: Factory[Long, C]): C = {
      val b = fac.newBuilder
      forEachOrdered(b += _)
      b.result()
    }
  }
}
