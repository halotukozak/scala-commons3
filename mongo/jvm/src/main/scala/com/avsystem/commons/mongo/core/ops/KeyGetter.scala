package com.avsystem.commons
package mongo.core.ops

import com.avsystem.commons.mongo.{BsonRef, DocKey}

trait KeyGetter[-T] {
  def keyOf(t: T): String
}

object KeyGetter {
  given KeyGetter[BsonRef[?, ?]] with {
    override def keyOf(t: BsonRef[?, ?]): String = t.path
  }

  given KeyGetter[DocKey[?, ?]] with {
    override def keyOf(t: DocKey[?, ?]): String = t.key
  }
}
