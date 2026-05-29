package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.explicitGenerics
// Polymorphic MongoDataCompanion (for generic types) not ported to scala-3.
// Scala-3 named-tuple based MacroInstances cannot hold polymorphic methods like
// `def codec[T: GenCodec]: GenObjectCodec[D[T]]`.
// Use per-instantiation MongoDataCompanion[D[Concrete]] or define codecs manually.

@deprecated(
  "MongoPolyAdtInstances not ported to scala-3 (polymorphic instance methods unsupported)",
  "scala-3 migration",
)
trait MongoPolyAdtInstances[D[_]]

@deprecated("AbstractMongoPolyDataCompanion not ported to scala-3", "scala-3 migration")
abstract class AbstractMongoPolyDataCompanion[Implicits, D[_]]

@deprecated("MongoPolyDataCompanion not ported to scala-3", "scala-3 migration")
abstract class MongoPolyDataCompanion[D[_]] extends AbstractMongoPolyDataCompanion[Nothing, D]
