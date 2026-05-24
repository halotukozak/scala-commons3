package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.BsonGenCodecs
import com.avsystem.commons.serialization.GenCodec.TransformedCodec
import com.avsystem.commons.serialization.{GenCodec, TransparentWrapperCompanion}
import org.bson.types.ObjectId

/** Base class for companion objects of wrappers ("newtypes") over BSON [[ObjectId]] - the default type of MongoDB
  * document ID. These wrappers usually serve to make the code more readable and typesafe. Because they are
  * "transparent", wrapping is not visible in any way after serialization.
  *
  * Example:
  * {{{
  *   import org.bson.types.ObjectId
  *
  *   case class MyEntityId(id: ObjectId) extends AnyVal
  *   object MyEntityId extends ObjectIdWrapperCompanion[MyEntityId]
  * }}}
  */
import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.serialization.TransparentWrapping

abstract class ObjectIdWrapperCompanion[ID](using MacroInstances[Unit, (tw: TransparentWrapping[ObjectId, ID])])
  extends TransparentWrapperCompanion[ObjectId, ID] {

  /** Generates new random, unique ID value.
    */
  def get(): ID = wrap(ObjectId.get())

  given idCodec: GenCodec[ID] = {
    val tw = summon[TransparentWrapping[ObjectId, ID]]
    new TransformedCodec(BsonGenCodecs.objectIdCodec, tw.unwrap, tw.wrap)
  }
}
