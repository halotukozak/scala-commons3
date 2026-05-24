package com.avsystem.commons
package mongo

import com.avsystem.commons.mongo.core.ops.{BsonRefFiltering, BsonRefIterableFiltering, BsonRefIterableUpdating, BsonRefSorting, BsonRefUpdating}
import com.avsystem.commons.serialization.RawRef.Field
import com.avsystem.commons.serialization.{GenCodec, GenRef}

case class BsonRef[S, T](path: String, codec: GenCodec[T], getter: S => T) {
  def apply(s: S): T = getter(s)

  def andThen[T0](other: BsonRef[T, T0]): BsonRef[S, T0] = {
    val newPath = List(path, other.path).filter(_.nonEmpty).mkString(BsonRef.BsonKeySeparator)
    BsonRef(newPath, other.codec, getter.andThen(other.getter))
  }

  def compose[S0](other: BsonRef[S0, S]): BsonRef[S0, T] =
    other.andThen(this)
}
object BsonRef {
  val BsonKeySeparator = "."

  def identity[S](using codec: GenCodec[S]): BsonRef[S, S] = BsonRef("", codec, s => s)
  def create[S]: Creator[S] = new Creator[S] {}

  trait Creator[S] {
    type Ref[T] = BsonRef[S, T]

    inline def ref[T](inline fun: S => T)(using GenCodec[T]): BsonRef[S, T] =
      BsonRef(GenRef.create[S].ref[T](fun))
  }

  def apply[S, T](genRef: GenRef[S, T])(using codec: GenCodec[T]): BsonRef[S, T] = {
    val path = genRef.rawRef.normalize
      .map { case Field(name) =>
        KeyEscaper.escape(name)
      }
      .mkString(BsonKeySeparator)

    BsonRef(path, codec, genRef.fun)
  }

  given [S, E, C[T] <: Iterable[T]] => GenCodec[E] => Conversion[BsonRef[S, C[E]], BsonRefIterableUpdating[E, C]] =
    bsonRef => new BsonRefIterableUpdating[E, C](bsonRef)
  given [S, T] => Conversion[BsonRef[S, T], BsonRefUpdating[T]] = new BsonRefUpdating(_)
  given [S, T] => Conversion[BsonRef[S, T], BsonRefSorting[T]] = new BsonRefSorting(_)
  given [S, E, C[T] <: Iterable[T]] => GenCodec[E] => Conversion[BsonRef[S, C[E]], BsonRefIterableFiltering[E, C]] =
    bsonRef => new BsonRefIterableFiltering[E, C](bsonRef)
  given [S, T] => Conversion[BsonRef[S, T], BsonRefFiltering[T]] = new BsonRefFiltering(_)

  @deprecated("Use summon[Conversion[BsonRef[S, C[E]], BsonRefIterableUpdating[E, C]]] or rely on implicit conversion", since = "scala-3")
  def bsonRefIterableUpdating[S, E: GenCodec, C[T] <: Iterable[T]](bsonRef: BsonRef[S, C[E]]): BsonRefIterableUpdating[E, C] =
    new BsonRefIterableUpdating[E, C](bsonRef)
  @deprecated("Use summon[Conversion[BsonRef[S, T], BsonRefUpdating[T]]] or rely on implicit conversion", since = "scala-3")
  def bsonRefUpdating[S, T](bsonRef: BsonRef[S, T]): BsonRefUpdating[T] = new BsonRefUpdating(bsonRef)
  @deprecated("Use summon[Conversion[BsonRef[S, T], BsonRefSorting[T]]] or rely on implicit conversion", since = "scala-3")
  def bsonRefSorting[S, T](bsonRef: BsonRef[S, T]): BsonRefSorting[T] = new BsonRefSorting(bsonRef)
  @deprecated("Use summon[Conversion[BsonRef[S, C[E]], BsonRefIterableFiltering[E, C]]] or rely on implicit conversion", since = "scala-3")
  def bsonRefIterableFiltering[S, E: GenCodec, C[T] <: Iterable[T]](bsonRef: BsonRef[S, C[E]]): BsonRefIterableFiltering[E, C] =
    new BsonRefIterableFiltering[E, C](bsonRef)
  @deprecated("Use summon[Conversion[BsonRef[S, T], BsonRefFiltering[T]]] or rely on implicit conversion", since = "scala-3")
  def bsonRefFiltering[S, T](bsonRef: BsonRef[S, T]): BsonRefFiltering[T] = new BsonRefFiltering(bsonRef)
}
