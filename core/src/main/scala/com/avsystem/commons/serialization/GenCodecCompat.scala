package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.{Bytes, Timestamp}

import java.util.UUID

trait GenCodecCompat { this: GenCodec.type =>
  @deprecated("Use given instance directly", since = "3.0.0")
  val NothingCodec: GenCodec[Nothing] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val NullCodec: GenCodec[Null] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val UnitCodec: GenCodec[Unit] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val VoidCodec: GenCodec[Void] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val BooleanCodec: GenCodec[Boolean] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val CharCodec: GenCodec[Char] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val ByteCodec: GenCodec[Byte] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val ShortCodec: GenCodec[Short] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val IntCodec: GenCodec[Int] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val LongCodec: GenCodec[Long] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val FloatCodec: GenCodec[Float] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val DoubleCodec: GenCodec[Double] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val BigIntCodec: GenCodec[BigInt] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val BigDecimalCodec: GenCodec[BigDecimal] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JBooleanCodec: GenCodec[JBoolean] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JCharacterCodec: GenCodec[JCharacter] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JByteCodec: GenCodec[JByte] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JShortCodec: GenCodec[JShort] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JIntegerCodec: GenCodec[JInteger] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JLongCodec: GenCodec[JLong] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JFloatCodec: GenCodec[JFloat] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JDoubleCodec: GenCodec[JDouble] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JBigIntegerCodec: GenCodec[JBigInteger] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JBigDecimalCodec: GenCodec[JBigDecimal] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val JDateCodec: GenCodec[JDate] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val StringCodec: GenCodec[String] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val SymbolCodec: GenCodec[Symbol] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val ArrayByteCodec: GenCodec[Array[Byte]] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val UUIDCodec: GenCodec[UUID] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val TimestampCodec: GenCodec[Timestamp] = summon
  @deprecated("Use given instance directly", since = "3.0.0")
  val BytesCodec: GenCodec[Bytes] = summon

  @deprecated("Use GenCodec.deriveRecursively instead", since = "3.0.0")
  inline def materializeRecursively[T]: GenCodec[T] = deriveRecursively[T]
}
