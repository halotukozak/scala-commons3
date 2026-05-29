package com.avsystem.commons.serialization
trait GenCodecCreates { this: GenCodec.type =>
  def create[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] = new GenCodec[T] {
    def write(output: Output, value: T): Unit = writeFun(output, value)
    def read(input: Input): T = readFun(input)
  }
  def makeLazy[T](codec: => GenCodec[T]): GenCodec[T] = new GenCodec[T] {
    private lazy val underlying = codec
    def read(input: Input): T = underlying.read(input)
    def write(output: Output, value: T): Unit = underlying.write(output, value)
  }
  def transformed[T, R: GenCodec](toRaw: T => R, fromRaw: R => T): GenCodec[T] =
    new TransformedCodec[T, R](GenCodec[R], toRaw, fromRaw)
  def createNullable[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T | Null] = new NullableCodec[T] {
    def readNonNull(input: Input): T = readFun(input)
    def writeNonNull(output: Output, value: T): Unit = writeFun(output, value)
  }
  def createString[T](readFun: String => T, writeFun: T => String): GenCodec[T] =
    createSimple(i => readFun(i.readString()), (o, v) => o.writeString(writeFun(v)))
  def createSimple[T](readFun: SimpleInput => T, writeFun: (SimpleOutput, T) => Any): GenCodec[T] = new SimpleCodec[T] {
    def readSimple(input: SimpleInput): T = readFun(input)
    def writeSimple(output: SimpleOutput, value: T): Unit = writeFun(output, value)
  }
  def createList[T](readFun: ListInput => T, writeFun: (ListOutput, T) => Any): GenCodec[T] = new ListCodec[T] {
    def readList(input: ListInput): T = readFun(input)
    def writeList(output: ListOutput, value: T): Unit = writeFun(output, value)
  }

  /**
   * Helper method to manually implement a `GenCodec` that writes an object. NOTE: in most cases the easiest way to
   * have a custom object codec is to manually implement `apply` and `unapply`/`unapplySeq` methods in companion object
   * of your type or use [[fromApplyUnapplyProvider]] if the type comes from a third party code and you can't modify
   * its companion object.
   */
  def createObject[T](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any): GenObjectCodec[T] =
    new ObjectCodec[T] {
      def readObject(input: ObjectInput): T = readFun(input)
      def writeObject(output: ObjectOutput, value: T): Unit = writeFun(output, value)
    }

  // --- scala-2 source-compat shims ---
  // The `allowNull` parameter is ignored in scala-3; the underlying codecs already write/read
  // null transparently. Prefer the explicit `T | Null` form (`createNullable`) for new code.

  @deprecated("Use `create` (always null-safe) or `createNullable` for explicit T | Null", since = "3.0.0")
  def nullSafe[T](readFun: Input => T, writeFun: (Output, T) => Any, allowNull: Boolean): GenCodec[T] =
    create(readFun, writeFun)

  @deprecated("Use `createNullable` for explicit T | Null", since = "3.0.0")
  def nullable[T <: AnyRef](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    create(readFun, writeFun)

  @deprecated("Use `create` instead", since = "3.0.0")
  def nonNull[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    create(readFun, writeFun)

  @deprecated("Use `createString` instead", since = "3.0.0")
  def nullableString[T <: AnyRef](readFun: String => T, writeFun: T => String): GenCodec[T] =
    createString(readFun, writeFun)

  @deprecated("Use `createString` instead", since = "3.0.0")
  def nonNullString[T](readFun: String => T, writeFun: T => String): GenCodec[T] =
    createString(readFun, writeFun)

  @deprecated("Use `createSimple` without `allowNull`", since = "3.0.0")
  def createSimple[T](readFun: SimpleInput => T, writeFun: (SimpleOutput, T) => Any, allowNull: Boolean): GenCodec[T] =
    createSimple(readFun, writeFun)

  @deprecated("Use `createSimple` instead", since = "3.0.0")
  def nullableSimple[T <: AnyRef](readFun: SimpleInput => T, writeFun: (SimpleOutput, T) => Any): GenCodec[T] =
    createSimple(readFun, writeFun)

  @deprecated("Use `createSimple` instead", since = "3.0.0")
  def nonNullSimple[T](readFun: SimpleInput => T, writeFun: (SimpleOutput, T) => Any): GenCodec[T] =
    createSimple(readFun, writeFun)

  @deprecated("Use `createList` without `allowNull`", since = "3.0.0")
  def createList[T](readFun: ListInput => T, writeFun: (ListOutput, T) => Any, allowNull: Boolean): GenCodec[T] =
    createList(readFun, writeFun)

  @deprecated("Use `createList` instead", since = "3.0.0")
  def nullableList[T <: AnyRef](readFun: ListInput => T, writeFun: (ListOutput, T) => Any): GenCodec[T] =
    createList(readFun, writeFun)

  @deprecated("Use `createList` instead", since = "3.0.0")
  def nonNullList[T](readFun: ListInput => T, writeFun: (ListOutput, T) => Any): GenCodec[T] =
    createList(readFun, writeFun)

  @deprecated("Use `createObject` without `allowNull`", since = "3.0.0")
  def createObject[T](
    readFun: ObjectInput => T,
    writeFun: (ObjectOutput, T) => Any,
    allowNull: Boolean,
  ): GenObjectCodec[T] =
    createObject(readFun, writeFun)

  @deprecated("Use `createObject` instead", since = "3.0.0")
  def nullableObject[T <: AnyRef](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any): GenObjectCodec[T] =
    createObject(readFun, writeFun)

  @deprecated("Use `createObject` instead", since = "3.0.0")
  def nonNullObject[T](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any): GenObjectCodec[T] =
    createObject(readFun, writeFun)
}
