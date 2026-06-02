package com.avsystem.commons
package misc

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

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
