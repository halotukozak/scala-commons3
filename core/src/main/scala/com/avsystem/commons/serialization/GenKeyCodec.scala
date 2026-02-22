package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.mirror.{DerMirror, DerSubSingletonElem}
import com.avsystem.commons.misc.{Bytes, Timestamp}
import com.avsystem.commons.serialization.GenCodec.{ReadFailure, WriteFailure}

import java.util.UUID
import scala.annotation.implicitNotFound

/**
 * Typeclass which implements two-directional conversion between values of some type and field names used in
 * [[ObjectOutput.writeField]] and [[ObjectInput.nextField]] ([[FieldInput.fieldName]]). Every type which has a
 * natural, unambiguous string representation should have a `GenKeyCodec`.
 */
@implicitNotFound("No GenKeyCodec found for ${T}")
trait GenKeyCodec[T] {
  def read(key: String): T
  def write(value: T): String

  final def transform[U](onWrite: U => T, onRead: T => U): GenKeyCodec[U] =
    new GenKeyCodec.Transformed(this, onWrite, onRead)
}

object GenKeyCodec {
  def apply[T](using gkc: GenKeyCodec[T]): GenKeyCodec[T] = gkc

  @explicitGenerics
  def read[T](key: String)(using keyCodec: GenKeyCodec[T]): T = keyCodec.read(key)
  def write[T](value: T)(using keyCodec: GenKeyCodec[T]): String = keyCodec.write(value)

  def create[T](readFun: String => T, writeFun: T => String): GenKeyCodec[T] =
    new GenKeyCodec[T] {
      def read(key: String): T = readFun(key)
      def write(value: T): String = writeFun(value)
    }

  //
  //  def forTransparentWrapper[T: WeakTypeTag]: Tree = {
  //    val tpe = weakTypeOf[T].dealias
  //    val codecTpe = getType(tq"$GenKeyCodecCls[$tpe]")
  //    val (applyUnapply, param) = applyUnapplyFor(tpe) match {
  //      case Some(au @ ApplyUnapply(_, _, _, _, List(soleParam))) => (au, soleParam)
  //      case _ => abort(s"$tpe is not a case class (or case class-like type) with exactly one field")
  //    }
  //
  //    val wrappedCodecTpe = getType(tq"$GenKeyCodecCls[${param.typeSignature}]")
  //    val clue = s"Cannot materialize $codecTpe because of problem with parameter ${param.name}:\n"
  //    val wrappedCodec = inferCachedImplicit(wrappedCodecTpe, ErrorCtx(clue, param.pos)).reference(Nil)
  //
  //    val unwrapped =
  //      if (applyUnapply.standardCaseClass)
  //        q"value.${param.name.toTermName}"
  //      else
  //        q"""
  //          ${applyUnapply.typedCompanion}.unapply[..${tpe.typeArgs}](value)
  //            .getOrElse(throw new $SerializationPkg.GenCodec.WriteFailure(
  //              s"Cannot write $$tpeString, unapply failed for $$value"))
  //         """
  //
  //    q"""
  //      new $codecTpe {
  //        ..$cachedImplicitDeclarations
  //        def tpeString: $StringCls = ${tpe.toString}
  //        def read(key: $StringCls): $tpe = ${applyUnapply.mkApply(List(q"$wrappedCodec.read(key)"))}
  //        def write(value: $tpe): $StringCls = $wrappedCodec.write($unwrapped)
  //      }
  //     """
  //  }
  // }

  given GenKeyCodec[Boolean] = create(_.toBoolean, _.toString)
  given GenKeyCodec[Char] = create(_.charAt(0), _.toString)
  given GenKeyCodec[Byte] = create(_.toByte, _.toString)
  given GenKeyCodec[Short] = create(_.toShort, _.toString)
  given GenKeyCodec[Int] = create(_.toInt, _.toString)
  given GenKeyCodec[Long] = create(_.toLong, _.toString)
  given GenKeyCodec[BigInt] = create(BigInt(_), _.toString)
  given GenKeyCodec[JBoolean] = create(_.toBoolean, _.toString)
  given GenKeyCodec[JCharacter] = create(_.charAt(0), _.toString)
  given GenKeyCodec[JByte] = create(_.toByte, _.toString)
  given GenKeyCodec[JShort] = create(_.toShort, _.toString)
  given GenKeyCodec[JInteger] = create(_.toInt, _.toString)
  given GenKeyCodec[JLong] = create(_.toLong, _.toString)
  given GenKeyCodec[JBigInteger] = create(new JBigInteger(_), _.toString)
  given GenKeyCodec[String] = create(identity, identity)
  given GenKeyCodec[Symbol] = create(Symbol(_), _.name)
  given GenKeyCodec[UUID] = create(UUID.fromString, _.toString)
  given GenKeyCodec[Timestamp] = GenKeyCodec.create(Timestamp.parse, _.toString)
  given GenKeyCodec[Bytes] = GenKeyCodec.create(Bytes.fromBase64(_), _.base64)
  given [E <: Enum[E]] => (ct: ClassTag[E]) => GenKeyCodec[E] =
    GenKeyCodec.create(
      string => Enum.valueOf(ct.runtimeClass.asInstanceOf[Class[E]], string),
      e => e.name(),
    )
  // Warning! Changing the order of implicit params of this method causes divergent implicit expansion (WTF?)
  given [R, T] => (tw: TransparentWrapping[R, T]) => (wrappedCodec: GenKeyCodec[R]) => GenKeyCodec[T] =
    new Transformed(wrappedCodec, tw.unwrap, tw.wrap)
  inline def forSealedEnum[T: {DerMirror.SumOf as m, ClassTag}]: GenKeyCodec[T] = {
    type IsSingleton[X] = X match {
      case Singleton => true
      case _ => false
    }

    inline compiletime.erasedValue[Tuple.Filter[m.MirroredElemTypes, IsSingleton]] match {
      case _: EmptyTuple =>
      case _ => 
        throw Exception(compiletime.summonInline[TypeRepr[m.MirroredElemTypes]])
//        compiletime.error("GenKeyCodec does not support not singletons")
    }

    deriveForSum(
      compiletime.constValue[m.MirroredLabel],
      compiletime.constValueTuple[m.MirroredElemLabels].toArrayOf[String],
      m.mirroredElems.toArrayOf[DerSubSingletonElem.Of[T]].map(_.value),
    )
  }
  inline def forTransparentWrapper[R, T](
    using tw: TransparentWrapping[R, T],
    underlyingCodec: GenKeyCodec[R],
  ): GenKeyCodec[T] = new Transformed(underlyingCodec, tw.unwrap, tw.wrap)
  def makeLazy[T](codec: => GenKeyCodec[T]): GenKeyCodec[T] = new GenKeyCodec[T] {
    private lazy val underlying = codec
    def read(input: String): T = underlying.read(input)
    def write(value: T): String = underlying.write(value)
  }
  private def deriveForSum[T](
    typeRepr: String,
    names: Array[String],
    values: Array[T],
  ): GenKeyCodec[T] = new GenKeyCodec[T] {
    private val valueByName = names.zip(values).toMap
    private val nameByValue = values.zip(names).toMap

    override def read(key: String): T =
      valueByName.getOrElse(key, throw new ReadFailure(s"Cannot read $typeRepr, unknown object: $key"))
    override def write(value: T): String =
      nameByValue.getOrElse(value, throw new WriteFailure(s"Cannot write $typeRepr, unknown value: $value"))
  }
  final class Transformed[A, B](val wrapped: GenKeyCodec[B], onWrite: A => B, onRead: B => A) extends GenKeyCodec[A] {
    def read(key: String): A = {
      val wrappedValue = wrapped.read(key)
      try onRead(wrappedValue)
      catch {
        case NonFatal(cause) => throw new ReadFailure(s"onRead conversion failed", cause)
      }
    }

    def write(value: A): String = {
      val wrappedValue =
        try onWrite(value)
        catch {
          case NonFatal(cause) => throw new WriteFailure(s"onWrite conversion failed", cause)
        }
      wrapped.write(wrappedValue)
    }
  }
}
