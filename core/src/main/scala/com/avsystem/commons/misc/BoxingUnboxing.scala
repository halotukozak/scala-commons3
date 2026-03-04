package com.avsystem.commons
package misc

case class Boxing[-A, +B](fun: A => B) extends AnyVal
object Boxing extends LowPrioBoxing {
  def fromImplicitConv[A, B](using conv: A => B): Boxing[A, B] = Boxing(conv)

  @deprecatedName("BooleanBoxing", since = "3.0.0")
  given Boxing[Boolean, JBoolean] = fromImplicitConv
  @deprecatedName("ByteBoxing", since = "3.0.0")
  given Boxing[Byte, JByte] = fromImplicitConv
  @deprecatedName("CharBoxing", since = "3.0.0")
  given Boxing[Short, JShort] = fromImplicitConv
  @deprecatedName("IntBoxing", since = "3.0.0")
  given Boxing[Int, JInteger] = fromImplicitConv
  @deprecatedName("LongBoxing", since = "3.0.0")
  given Boxing[Long, JLong] = fromImplicitConv
  @deprecatedName("FloatBoxing", since = "3.0.0")
  given Boxing[Float, JFloat] = fromImplicitConv
  @deprecatedName("DoubleBoxing", since = "3.0.0")
  given Boxing[Double, JDouble] = fromImplicitConv
}
trait LowPrioBoxing { this: Boxing.type =>
  given [A] => Boxing[A, A] = Boxing(identity)
}

case class Unboxing[+A, -B](fun: B => A) extends AnyVal
object Unboxing extends LowPrioUnboxing {
  def fromImplicitConv[A, B](using conv: B => A): Unboxing[A, B] = Unboxing(conv)
  @deprecatedName("BooleanBoxing", since = "3.0.0")
  given Unboxing[Boolean, JBoolean] = fromImplicitConv
  @deprecatedName("ByteBoxing", since = "3.0.0")
  given Unboxing[Byte, JByte] = fromImplicitConv
  @deprecatedName("CharBoxing", since = "3.0.0")
  given Unboxing[Char, Char] = fromImplicitConv
  @deprecatedName("CharBoxing", since = "3.0.0")
  given Unboxing[Short, JShort] = fromImplicitConv
  @deprecatedName("IntBoxing", since = "3.0.0")
  given Unboxing[Int, JInteger] = fromImplicitConv
  @deprecatedName("LongBoxing", since = "3.0.0")
  given Unboxing[Long, JLong] = fromImplicitConv
  @deprecatedName("FloatBoxing", since = "3.0.0")
  given Unboxing[Float, JFloat] = fromImplicitConv
  @deprecatedName("DoubleBoxing", since = "3.0.0")
  given Unboxing[Double, JDouble] = fromImplicitConv
}
trait LowPrioUnboxing { this: Unboxing.type =>
  @deprecatedName("nullableBoxing", since = "3.0.0")
  given [A] => Unboxing[A, A] = Unboxing(identity)
}
