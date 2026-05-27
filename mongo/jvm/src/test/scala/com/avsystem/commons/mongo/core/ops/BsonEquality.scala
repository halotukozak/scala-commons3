package com.avsystem.commons
package mongo.core.ops

import org.bson.conversions.Bson
import org.scalactic.Equality

given bsonEquality: Equality[Bson] = {
  case (null, null) => true
  case (aBson, bBson: Bson) => aBson.toBsonDocument == bBson.toBsonDocument
  case _ => false
}
