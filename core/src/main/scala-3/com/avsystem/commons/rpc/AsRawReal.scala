package com.avsystem.commons
package rpc

import com.avsystem.commons.meta.Fallback
import com.avsystem.commons.misc.ImplicitNotFound
import made.*
import made.annotation.*

import scala.annotation.{implicitNotFound, nowarn}

@implicitNotFound("Cannot serialize ${Real} into ${Raw}, appropriate AsRaw instance not found")
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
trait AsRaw[Raw, Real] {
  def asRaw(real: Real): Raw
}
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
object AsRaw extends FallbackAsRaw with AsRawMacros {
  def apply[Raw, Real](using asRaw: AsRaw[Raw, Real]): AsRaw[Raw, Real] = asRaw

  @deprecated("use SAM syntax (lambda)", "2.0.0")
  def create[Raw, Real](asRawFun: Real => Raw): AsRaw[Raw, Real] = asRawFun(_)

  // deliberately not implicit so that each raw type can turn it into an implicit with appropriate priority if desired
  def fromTransparentWrapping[Wrapped, Raw, Real](
    using tw: TransparentWrapping[Wrapped, Real],
    forWrapped: AsRaw[Raw, Wrapped],
  ): AsRaw[Raw, Real] = real => forWrapped.asRaw(tw.unwrap(real))

  given [A] => AsRaw[A, A] = AsRawReal.identity[A]
  given [Raw, Real] => (asRaw: AsRaw[Raw, Real]) => AsRaw[Try[Raw], Try[Real]] =
    _.map(asRaw.asRaw)

  @implicitNotFound("#{forPlain}")
  implicit def notFoundForTry[Raw, Real](
    implicit forPlain: ImplicitNotFound[AsRaw[Raw, Real]],
  ): ImplicitNotFound[AsRaw[Try[Raw], Try[Real]]] = ImplicitNotFound()
}
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
trait FallbackAsRaw { this: AsRaw.type =>
  implicit def fromFallback[Raw, Real](implicit fallback: Fallback[AsRaw[Raw, Real]]): AsRaw[Raw, Real] =
    fallback.value
}

@implicitNotFound("Cannot deserialize ${Real} from ${Raw}, appropriate AsReal instance not found")
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
trait AsReal[Raw, Real] {
  def asReal(raw: Raw): Real
}
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
object AsReal extends FallbackAsReal with AsRealMacros {
  def apply[Raw, Real](using asReal: AsReal[Raw, Real]): AsReal[Raw, Real] = asReal

  @deprecated("use SAM syntax (lambda)", "2.0.0")
  def create[Raw, Real](asRealFun: Raw => Real): AsReal[Raw, Real] = asRealFun(_)

  // deliberately not implicit so that each raw type can turn it into an implicit with appropriate priority if desired
  def fromTransparentWrapping[Wrapped, Raw, Real](
    using tw: TransparentWrapping[Wrapped, Real],
    forWrapped: AsReal[Raw, Wrapped],
  ): AsReal[Raw, Real] = raw => tw.wrap(forWrapped.asReal(raw))

  implicit def identity[A]: AsReal[A, A] = AsRawReal.identity[A]
  implicit def forTry[Raw, Real](implicit asReal: AsReal[Raw, Real]): AsReal[Try[Raw], Try[Real]] =
    _.map(asReal.asReal)

  @implicitNotFound("#{forPlain}")
  implicit def notFoundForTry[Raw, Real](
    implicit forPlain: ImplicitNotFound[AsReal[Raw, Real]],
  ): ImplicitNotFound[AsReal[Try[Raw], Try[Real]]] = ImplicitNotFound()
}
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
trait FallbackAsReal { this: AsReal.type =>
  implicit def fromFallback[Raw, Real](implicit fallback: Fallback[AsReal[Raw, Real]]): AsReal[Raw, Real] =
    fallback.value
}

@implicitNotFound(
  "Cannot serialize and deserialize between ${Real} and ${Raw}, appropriate AsRawReal instance not found",
)
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
trait AsRawReal[Raw, Real] extends AsReal[Raw, Real] with AsRaw[Raw, Real]
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
object AsRawReal extends AsRawRealLowPrio with AsRawRealMacros {
  private val reusableIdentity = new AsRawReal[Any, Any] {
    def asReal(raw: Any): Any = raw
    def asRaw(real: Any): Any = real
  }
  def apply[Raw, Real](implicit asRawReal: AsRawReal[Raw, Real]): AsRawReal[Raw, Real] = asRawReal
  def create[Raw, Real](asRawFun: Real => Raw, asRealFun: Raw => Real): AsRawReal[Raw, Real] =
    new AsRawReal[Raw, Real] {
      def asRaw(real: Real): Raw = asRawFun(real)
      def asReal(raw: Raw): Real = asRealFun(raw)
    }

  implicit def identity[A]: AsRawReal[A, A] =
    reusableIdentity.asInstanceOf[AsRawReal[A, A]]

}
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
trait AsRawRealLowPrio extends FallbackAsRawReal { this: AsRawReal.type =>
  implicit def fromSeparateAsRealAndRaw[Raw, Real](implicit asRaw: AsRaw[Raw, Real], asReal: AsReal[Raw, Real])
    : AsRawReal[Raw, Real] = AsRawReal.create(asRaw.asRaw, asReal.asReal)
}
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
trait FallbackAsRawReal { this: AsRawReal.type =>
  implicit def fromFallback[Raw, Real](implicit fallback: Fallback[AsRawReal[Raw, Real]]): AsRawReal[Raw, Real] =
    fallback.value
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@nowarn("msg=deprecated")
object RpcMetadata extends RpcMetadataMacros {

  def nextInstance[T](it: Iterator[?], description: String): T =
    if (it.hasNext) it.next().asInstanceOf[T]
    else throw new NoSuchElementException(s"typeclass instance for $description was not provided")
}
