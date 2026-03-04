package com.avsystem.commons
package misc

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

trait BoxingCompat { this: Boxing.type =>
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val BooleanBoxing: Boxing[Boolean, JBoolean] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val ByteBoxing: Boxing[Byte, JByte] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val CharBoxing: Boxing[Short, JShort] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val IntBoxing: Boxing[Int, JInteger] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val LongBoxing: Boxing[Long, JLong] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val FloatBoxing: Boxing[Float, JFloat] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val DoubleBoxing: Boxing[Double, JDouble] = summon
}

trait LowPrioBoxingCompat { this: Boxing.type =>
  @deprecated("Use given instance directly", since = "3.0.0")
  def nullableBoxing[A]: Boxing[A, A] = summon
}

trait UnboxingCompat { this: Unboxing.type =>
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val BooleanBoxing: Unboxing[Boolean, JBoolean] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val ByteUnboxing: Unboxing[Byte, JByte] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val CharBoxing: Unboxing[Short, JShort] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val IntBoxing: Unboxing[Int, JInteger] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val LongBoxing: Unboxing[Long, JLong] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val FloatBoxing: Unboxing[Float, JFloat] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val DoubleBoxing: Unboxing[Double, JDouble] = summon
}

trait LowPrioUnboxingCompat { this: Unboxing.type =>
  @deprecated("Use given instance directly", since = "3.0.0")
  def nullableBoxing[A]: Unboxing[A, A] = summon
}

trait OptCompat { this: Opt.type =>
  @deprecated("Use given Conversion directly", since = "3.0.0")
  def opt2Iterable[A](opt: Opt[A]): Iterable[A] = summon[Conversion[Opt[A], Iterable[A]]](opt)
}

trait NOptCompat { this: NOpt.type =>
  @deprecated("Use given Conversion directly", since = "3.0.0")
  def opt2Iterable[A](opt: NOpt[A]): Iterable[A] = summon[Conversion[NOpt[A], Iterable[A]]](opt)
}

trait OptRefCompat { this: OptRef.type =>
  @deprecated("Use given Conversion directly", since = "3.0.0")
  def opt2Iterable[A](opt: OptRef[A]): Iterable[A] = summon[Conversion[OptRef[A], Iterable[A]]](opt)
}

trait TimestampCompat { this: Timestamp.type =>
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val conversions: Conversion[Timestamp, TimestampConversions] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val ordering: Ordering[Timestamp] = summon
}

trait TypeStringCompat { this: TypeString.type =>
  @deprecated("Use given instance directly", since = "3.0.0")
  def keyCodec[T]: GenKeyCodec[TypeString[T]] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  def codec[T]: GenCodec[TypeString[T]] = summon
}

trait JavaClassNameCompat { this: JavaClassName.type =>
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val NothingClassName: JavaClassName[Nothing] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val NothingArrayClassName: JavaClassName[Array[Nothing]] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val UnitClassName: JavaClassName[Unit] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val BooleanClassName: JavaClassName[Boolean] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val ByteClassName: JavaClassName[Byte] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val ShortClassName: JavaClassName[Short] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val IntClassName: JavaClassName[Int] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val LongClassName: JavaClassName[Long] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val FloatClassName: JavaClassName[Float] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val DoubleClassName: JavaClassName[Double] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val CharClassName: JavaClassName[Char] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val AnyClassName: JavaClassName[Any] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val AnyValClassName: JavaClassName[AnyVal] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val keyCodec: GenKeyCodec[JavaClassName[?]] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val codec: GenCodec[JavaClassName[?]] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  def arrayClassName[T: JavaClassName]: JavaClassName[Array[T]] = summon
}

trait NamedEnumCompanionCompat[T <: NamedEnum] { this: NamedEnumCompanion[T] =>
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val keyCodec: GenKeyCodec[T] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val codec: GenCodec[T] = summon
}

trait OrderedEnumCompat { this: OrderedEnum.type =>
  @deprecated("Use given instance directly", since = "3.0.0")
  def ordering[T <: OrderedEnum]: Ordering[T] = summon
}

trait ValueEnumCompanionCompat[T <: ValueEnum] { this: ValueEnumCompanion[T] =>
  @deprecated("Use given instance directly", since = "3.0.0")
  lazy val ordering: Ordering[T] = summon
}
