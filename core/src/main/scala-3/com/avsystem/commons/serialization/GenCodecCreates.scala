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
}
