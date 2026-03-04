package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.{AnnotationAggregate, explicitGenerics}
import com.avsystem.commons.derivation.DeferredInstance
import com.avsystem.commons.jiop.JFactory
import com.avsystem.commons.meta.*
import com.avsystem.commons.misc.{Bytes, Timestamp}
import made.*
import made.annotation.*

import java.util.UUID
import scala.NamedTuple.*
import scala.annotation.{implicitNotFound, tailrec, targetName}
import scala.collection.{Factory, mutable}

/**
 * Type class for types that can be serialized to [[Output]] (format-agnostic "output stream") and deserialized from
 * [[Input]] (format-agnostic "input stream"). `GenCodec` is supposed to capture generic structure of serialized
 * objects, without being bound to particular format like JSON. The actual format is determined by implementation of
 * [[Input]] and [[Output]].
 *
 * There are convenient macros for automatic derivation of [[GenCodec]] instances (`materialize` and
 * `materializeRecursively`). However, [[GenCodec]] instances still need to be explicitly declared and won't be derived
 * "automagically".
 */
@implicitNotFound("No GenCodec found for ${T}")
trait GenCodec[T] {

  /**
   * Deserializes a value of type `T` from an [[Input]].
   */
  def read(input: Input): T

  /**
   * Serializes a value of type `T` into an [[Output]].
   */
  def write(output: Output, value: T): Unit

  /**
   * Transforms this codec into a codec of other type using a bidirectional conversion between the original and new
   * type.
   */
  final def transform[U](onWrite: U => T, onRead: T => U): GenCodec[U] =
    new GenCodec.TransformedCodec[U, T](this, onWrite, onRead)
}

object GenCodec
  extends GenCodecDerivation, GenCodecImpl, GenCodecCreates, GenCodecFailures, GenCodecJavaBuilder, GenCodecUtils {
  final val DefaultCaseField = "_case"
  def apply[T](using codec: GenCodec[T]): GenCodec[T] = codec

  inline def applyUnapplyCodec[T]: ApplyUnapplyCodec[T] = ???

  @deprecated("Use GenCodec.transform with named tuple instead", since = "3.0.0")
  def fromApplyUnapplyProvider[T]: Any => GenCodec[T] = _ => ???

  inline given [Tup <: Tuple] => GenCodec[Tup] = mkTupleCodec(
    compiletime.summonAll[Tuple.Map[Tup, GenCodec]],
  )

  def materialize[T]: GenCodec[T] = ???
  @explicitGenerics
  def read[T: GenCodec](input: Input): T =
    apply[T].read(input)
  def write[T: GenCodec](output: Output, value: T): Unit =
    apply[T].write(output, value)

  def fromKeyCodec[T](using keyCodec: GenKeyCodec[T]): GenCodec[T] = create(
    input => keyCodec.read(input.readSimple().readString()),
    (output, value) => output.writeSimple().writeString(keyCodec.write(value)),
  )

  // for some reason cannot be Made.SumOf as parameter
  inline def forSealedEnum[T: {Made.Of as m, ClassTag}]: GenCodec[T] = inline m match {
    case given Made.SumOf[T] => GenCodec.fromKeyCodec(using GenKeyCodec.forSealedEnum[T])
    case _ => compiletime.error("Unsupported derivation for GenCodec.forSealedEnum")
  }

  @tailrec def underlyingCodec(codec: GenCodec[?]): GenCodec[?] = codec match {
    case tc: TransformedCodec[_, _] => underlyingCodec(tc.wrapped)
    case _ => codec
  }
  @deprecatedName("NothingCodec", since = "3.0.0")
  given GenCodec[Nothing] =
    create[Nothing](_ => throw new ReadFailure("read Nothing"), (_, _) => throw new WriteFailure("write Nothing"))
  @deprecatedName("NullCodec", since = "3.0.0")
  given GenCodec[Null] =
    create[Null](i => if (i.readNull()) null else notNull, (o, _) => o.writeNull())
  @deprecatedName("UnitCodec", since = "3.0.0")
  given GenCodec[Unit] =
    create[Unit](i => if (i.readNull()) () else notNull, (o, _) => o.writeNull())
  @deprecatedName("VoidCodec", since = "3.0.0")
  given GenCodec[Void] =
    create[Void](i => if (i.readNull()) null.asInstanceOf[Void] else notNull, (o, _) => o.writeNull())
  @deprecatedName("BooleanCodec", since = "3.0.0")
  given GenCodec[Boolean] = createSimple(_.readBoolean(), _ `writeBoolean` _)
  @deprecatedName("CharCodec", since = "3.0.0")
  given GenCodec[Char] = createSimple(_.readChar(), _ `writeChar` _)
  @deprecatedName("ByteCodec", since = "3.0.0")
  given GenCodec[Byte] = createSimple(_.readByte(), _ `writeByte` _)
  @deprecatedName("ShortCodec", since = "3.0.0")
  given GenCodec[Short] = createSimple(_.readShort(), _ `writeShort` _)
  @deprecatedName("IntCodec", since = "3.0.0")
  given GenCodec[Int] = createSimple(_.readInt(), _ `writeInt` _)
  @deprecatedName("LongCodec", since = "3.0.0")
  given GenCodec[Long] = createSimple(_.readLong(), _ `writeLong` _)
  @deprecatedName("FloatCodec", since = "3.0.0")
  given GenCodec[Float] = createSimple(_.readFloat(), _ `writeFloat` _)
  @deprecatedName("DoubleCodec", since = "3.0.0")
  given GenCodec[Double] = createSimple(_.readDouble(), _ `writeDouble` _)
  @deprecatedName("BigIntCodec", since = "3.0.0")
  given GenCodec[BigInt] = createSimple(_.readBigInt(), _ `writeBigInt` _)
  @deprecatedName("BigDecimalCodec", since = "3.0.0")
  given GenCodec[BigDecimal] = createSimple(_.readBigDecimal(), _ `writeBigDecimal` _)
  @deprecatedName("JBooleanCodec", since = "3.0.0")
  given GenCodec[JBoolean] = createSimple(_.readBoolean(), _ `writeBoolean` _)
  @deprecatedName("JCharacterCodec", since = "3.0.0")
  given GenCodec[JCharacter] = createSimple(_.readChar(), _ `writeChar` _)
  @deprecatedName("JByteCodec", since = "3.0.0")
  given GenCodec[JByte] = createSimple(_.readByte(), _ `writeByte` _)
  @deprecatedName("JShortCodec", since = "3.0.0")
  given GenCodec[JShort] = createSimple(_.readShort(), _ `writeShort` _)
  @deprecatedName("JIntegerCodec", since = "3.0.0")
  given GenCodec[JInteger] = createSimple(_.readInt(), _ `writeInt` _)
  @deprecatedName("JLongCodec", since = "3.0.0")
  given GenCodec[JLong] = createSimple(_.readLong(), _ `writeLong` _)
  @deprecatedName("JFloatCodec", since = "3.0.0")
  given GenCodec[JFloat] = createSimple(_.readFloat(), _ `writeFloat` _)
  @deprecatedName("JDoubleCodec", since = "3.0.0")
  given GenCodec[JDouble] = createSimple(_.readDouble(), _ `writeDouble` _)
  @deprecatedName("JBigIntegerCodec", since = "3.0.0")
  given GenCodec[JBigInteger] = createSimple(_.readBigInt().bigInteger, (o, v) => o.writeBigInt(BigInt(v)))
  @deprecatedName("JBigDecimalCodec", since = "3.0.0")
  given GenCodec[JBigDecimal] = createSimple(_.readBigDecimal().bigDecimal, (o, v) => o.writeBigDecimal(BigDecimal(v)))
  @deprecatedName("JDateCodec", since = "3.0.0")
  given GenCodec[JDate] = createSimple(i => new JDate(i.readTimestamp()), (o, d) => o.writeTimestamp(d.getTime))
  @deprecatedName("StringCodec", since = "3.0.0")
  given GenCodec[String] = createSimple(_.readString(), _ `writeString` _)
  @deprecatedName("SymbolCodec", since = "3.0.0")
  given GenCodec[Symbol] = createSimple(i => Symbol(i.readString()), (o, s) => o.writeString(s.name))
  @deprecatedName("ArrayByteCodec", since = "3.0.0")
  given GenCodec[Array[Byte]] = createSimple(_.readBinary(), _ `writeBinary` _)
  @deprecatedName("UUIDCodec", since = "3.0.0")
  given GenCodec[UUID] = createSimple(i => UUID.fromString(i.readString()), (o, v) => o.writeString(v.toString))
  @deprecatedName("TimestampCodec", since = "3.0.0")
  given GenCodec[Timestamp] =
    GenCodec.createSimple(i => Timestamp(i.readTimestamp()), (o, t) => o.writeTimestamp(t.millis))
  @deprecatedName("BytesCodec", since = "3.0.0")
  given GenCodec[Bytes] = GenCodec.createSimple(i => Bytes(i.readBinary()), (o, b) => o.writeBinary(b.bytes))
  given [T] => (codec: GenCodec[T]) => GenCodec[T | Null] = createNullable(codec.read, codec.write)
  given [T: {ClassTag, GenCodec}] => GenCodec[Array[T]] = createList[Array[T]](
    _.iterator(read[T]).toArray[T],
    (lo, arr) => {
      lo.declareSize(arr.length)
      @tailrec def loop(idx: Int): Unit =
        if (idx < arr.length) {
          GenCodec.write(lo.writeElement(), arr(idx))
          loop(idx + 1)
        }
      loop(0)
    },
  )
  @targetName("seqCodec")
  given [C[X] <: BSeq[X], T: GenCodec] => Factory[T, C[T]] => GenCodec[C[T]] =
    createList[C[T]](_.collectTo[T, C[T]], (lo, c) => c.writeToList(lo))
  given [C[X] <: BSet[X], T: GenCodec] => Factory[T, C[T]] => GenCodec[C[T]] =
    createList[C[T]](_.collectTo[T, C[T]], (lo, c) => c.writeToList(lo))
  given [C[X] <: JCollection[X], T: GenCodec] => JFactory[T, C[T]] => GenCodec[C[T]] =
    createList[C[T]](_.collectTo[T, C[T]], (lo, c) => c.asScala.writeToList(lo))
  given [M[X, Y] <: BMap[X, Y], K: GenKeyCodec, V: GenCodec] => Factory[(K, V), M[K, V]] => GenObjectCodec[M[K, V]] =
    createObject[M[K, V]](_.collectTo[K, V, M[K, V]], (oo, value) => value.writeToObject(oo))
  given [M[X, Y] <: JMap[X, Y], K: GenKeyCodec, V: GenCodec] => JFactory[(K, V), M[K, V]] => GenObjectCodec[M[K, V]] =
    createObject[M[K, V]](_.collectTo[K, V, M[K, V]], (oo, value) => value.asScala.writeToObject(oo))
  given [T: GenCodec] => GenCodec[Option[T]] = create[Option[T]](
    input =>
      if (input.legacyOptionEncoding) {
        val li = input.readList()
        val res = if (li.hasNext) Some(read[T](li.nextElement())) else None
        li.skipRemaining()
        res
      } else if (input.readNull()) None
      else Some(read[T](input)),
    (output, valueOption) =>
      if (output.legacyOptionEncoding) {
        val lo = output.writeList()
        valueOption.foreach(v => write[T](lo.writeElement(), v))
        lo.finish()
      } else
        valueOption match {
          case Some(v) => write[T](output, v)
          case None => output.writeNull()
        },
  )
  given [T: GenCodec] => GenCodec[NOpt[T]] =
    new TransformedCodec[NOpt[T], Option[T]](summon, _.toOption, _.toNOpt)
  given [T: GenCodec] => GenCodec[Opt[T]] =
    create[Opt[T]](
      i => if (i.readNull()) Opt.Empty else Opt(read[T](i)),
      (o, vo) =>
        vo match {
          case Opt(v) => write[T](o, v)
          case Opt.Empty => o.writeNull()
        },
    )
  given [T: GenCodec] => GenCodec[OptArg[T]] =
    new TransformedCodec[OptArg[T], Opt[T]](summon, _.toOpt, _.toOptArg)
  given [T: GenCodec] => GenCodec[OptRef[T]] =
    new TransformedCodec[OptRef[T], Opt[T]](summon, _.toOpt, _.toOptRef)
  given [A: GenCodec, B: GenCodec] => GenCodec[Either[A, B]] = createObject(
    oi => {
      val fi = oi.nextField()
      fi.fieldName match {
        case "Left" => Left(read[A](fi))
        case "Right" => Right(read[B](fi))
        case name => throw new ReadFailure(s"Expected field 'Left' or 'Right', got $name")
      }
    },
    (oo, v) => {
      oo.declareSize(1)
      v match {
        case Left(a) => write[A](oo.writeField("Left"), a)
        case Right(b) => write[B](oo.writeField("Right"), b)
      }
    },
  )
  given [E <: Enum[E]: ClassTag] => GenCodec[E] = createSimple(
    in => Enum.valueOf(classTag[E].runtimeClass.asInstanceOf[Class[E]], in.readString()),
    (out, value) => out.writeString(value.name),
  )
  // Warning! Changing the order of implicit params of this method causes divergent implicit expansion (WTF?)
  given [R, T] => (tw: TransparentWrapping[R, T]) => (wrappedCodec: GenCodec[R]) => GenCodec[T] =
    new TransformedCodec(wrappedCodec, tw.unwrap, tw.wrap)
  given [T] => (fallback: Fallback[GenCodec[T]]) => GenCodec[T] =
    fallback.value

  inline given [T: Made.Of] => (AllowRecursiveDerivation.type) => GenCodec[T] = GenCodec.derived[T]
  inline given [T: Made.Of] => (AllowDerivation[GenCodec[T]]) => GenCodec[T] = GenCodec.derived[T]

  private def mkTupleCodec[Tup <: Tuple](elementCodecs: Tuple.Map[Tup, GenCodec]): GenCodec[Tup] = new ListCodec[Tup] {
    override def readList(input: ListInput): Tup =
      elementCodecs
        .map[[X] =>> Any]([C] => (codec: C) => codec.asInstanceOf[GenCodec[?]].read(input.nextElement()))
        .asInstanceOf[Tup]

    override def writeList(output: ListOutput, value: Tup): Unit = {
      output.declareSize(elementCodecs.size)
      elementCodecs
        .zip(value)
        .map[[X] =>> Unit]([CE] =>
          (ce: CE) =>
            ce match {
              case (codec: GenCodec[Any] @unchecked, element) => codec.write(output.writeElement(), element)
            },
        )

    }
  }

  private def notNull: Nothing = throw new ReadFailure("not null")
}
