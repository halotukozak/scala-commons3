package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.mongo.BsonGenCodecs
import com.avsystem.commons.serialization.GenObjectCodec

import scala.annotation.{compileTimeOnly, implicitNotFound}

type MongoAdtInstances[T] = (codec: GenObjectCodec[T], format: MongoAdtFormat[T])

type MongoEntityInstances[E <: BaseMongoEntity] =
  (codec: GenObjectCodec[E], format: MongoAdtFormat[E], meta: MongoEntityMeta[E])

/** Provides additional static validation for `as`, `is` and `ref` macros from [[DataTypeDsl]]. Catches mistakes when
  * someone forgets to use [[MongoDataCompanion]] or [[MongoEntityCompanion]] for its case class or sealed hierarchy.
  */
@implicitNotFound("${T} is an opaque data type - does it have a companion that extends MongoDataCompanion?")
sealed trait IsMongoAdtOrSubtype[T]

type IDOf[E <: BaseMongoEntity] = E match {
  case MongoEntity[id] => id
  case AutoIdMongoEntity[id] => id
}
object IsMongoAdtOrSubtype {
  private object instance extends IsMongoAdtOrSubtype[Any]
  def witness[T]: IsMongoAdtOrSubtype[T] = instance.asInstanceOf[IsMongoAdtOrSubtype[T]]
}

sealed trait BaseMongoCompanion[T] extends DataTypeDsl[T] {
  def codec: GenObjectCodec[T]
  def format: MongoAdtFormat[T]
  given GenObjectCodec[T] = codec
  given MongoAdtFormat[T] = format

  given [C <: T] => IsMongoAdtOrSubtype[C] = IsMongoAdtOrSubtype.witness[C]

  extension (value: T) {
    @explicitGenerics
    @compileTimeOnly("the .as[Subtype] construct can only be used inside lambda passed to .ref(...) macro")
    def as[C <: T]: C = sys.error("stub")
  }

  final lazy val SelfRef: MongoRef[T, T] = MongoRef.RootRef(format)
}

abstract class AbstractMongoDataCompanion[Implicits, E](
  implicits: Implicits
)(using instances: MacroInstances[Implicits, MongoAdtInstances[E]]
) extends BaseMongoCompanion[E] {
  override lazy val codec: GenObjectCodec[E] = instances(implicits, this).codec
  override lazy val format: MongoAdtFormat[E] = instances(implicits, this).format
}

abstract class AbstractMongoEntityCompanion[Implicits, E <: BaseMongoEntity](
  implicits: Implicits
)(using instances: MacroInstances[Implicits, MongoEntityInstances[E]]
) extends BaseMongoCompanion[E] {
  override lazy val codec: GenObjectCodec[E] = instances(implicits, this).codec
  override lazy val format: MongoAdtFormat[E] = instances(implicits, this).format
  given meta: MongoEntityMeta[E] = instances(implicits, this).meta

  type ID = IDOf[E]

  final val IdRef: Ref[ID] = meta.idRef
}

/** Base class for companion objects of types that represent inner documents of MongoDB entities. Just like entities,
  * inner documents may be case classes or sealed hierarchies with `@flatten` annotation.
  *
  * NOTE: It is enough for a MongoDB field type to have just `GenCodec` defined (i.e. you can get away with using
  * `HasGenCodec` instead of `MongoDataCompanion`). However, data type which only has codec will be considered opaque
  * and you won't be able to reference its inner fields in filters, updates, indices, etc.
  */
abstract class MongoDataCompanion[E](using
  instances: MacroInstances[BsonGenCodecs.type, MongoAdtInstances[E]]
) extends AbstractMongoDataCompanion[BsonGenCodecs.type, E](BsonGenCodecs)

/** Base class for companion objects of types representing MongoDB entities. Entities may be case classes or sealed
  * hierarchies with `@flatten` annotation. They must extend [[MongoEntity]].
  */
abstract class MongoEntityCompanion[E <: BaseMongoEntity](using
  instances: MacroInstances[BsonGenCodecs.type, MongoEntityInstances[E]]
) extends AbstractMongoEntityCompanion[BsonGenCodecs.type, E](BsonGenCodecs)
