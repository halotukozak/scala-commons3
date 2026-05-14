package com.avsystem.commons
package jiop

import com.avsystem.commons.misc.TimestampConversions

import java.util.Comparator
import java.util.concurrent.Callable
import java.{lang as jl, math as jm, util as ju}

trait JBasicUtils {
  inline def jRunnable(inline code: => Any): Runnable = () => code
  inline def jCallable[T](inline expr: => T): Callable[T] = () => expr
  inline def jComparator[T](inline cmp: (T, T) => Int): Comparator[T] = cmp(_, _)

  given Conversion[JDate, TimestampConversions] = date => TimestampConversions(date.getTime)

  type JByte = jl.Byte
  type JShort = jl.Short
  type JInteger = jl.Integer
  type JLong = jl.Long
  type JFloat = jl.Float
  type JDouble = jl.Double
  type JBoolean = jl.Boolean
  type JCharacter = jl.Character
  type JBigInteger = jm.BigInteger
  type JBigDecimal = jm.BigDecimal
  type JDate = ju.Date
  type JNumber = jl.Number
  type JVoid = jl.Void
  type JEnum[E <: jl.Enum[E]] = jl.Enum[E]
  type JStringBuilder = jl.StringBuilder
}
