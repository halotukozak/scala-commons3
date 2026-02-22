package com.avsystem.commons
package serialization

import com.avsystem.commons.mirror.name
import com.avsystem.commons.misc.HasAnnotation

opaque type TypeRepr[T] <: String = String
object TypeRepr {
  inline given [T] => TypeRepr[T] = ${ deriveImpl[T] }

  private def deriveImpl[T: Type](using quotes: Quotes): Expr[TypeRepr[T]] = {
    Expr(quotes.reflect.TypeRepr.of[T].show)
  }
}
