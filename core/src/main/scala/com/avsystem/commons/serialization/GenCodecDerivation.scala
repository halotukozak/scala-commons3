package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.*
import made.*
trait GenCodecDerivation { this: GenCodec.type =>
  inline def derived[T]: GenCodec[T] = {
    given deferred: DeferredCodec[T] = new DeferredCodec[T]
    val underlying = unsafeDerived[T](using compiletime.summonInline[Made.Of[T]])
    deferred.underlying = underlying
    underlying
  }

  @deprecatedName("Use GenCodec.deriveRecursibely instead", since = "3.0.0")
  inline def materializeRecursively[T]: GenCodec[T] = deriveRecursively[T]

  inline def deriveRecursively[T]: GenCodec[T] = {
    given AllowRecursiveDerivation.type = AllowRecursiveDerivation
    derived[T]
  }

  inline private def unsafeDerived[T: Made.Of as made]: GenCodec[T] = {
    val label = compiletime.constValue[made.MirroredLabel]
    val generatedNames = compiletime
      .constValueTuple[Tuple.Map[made.GeneratedElems, [Elem] =>> Elem match { case MadeElem.LabelOf[label] => label }]]
      .toArrayOf[String]
    val generatedExtractors = made.generatedElems.toArrayOf[GeneratedMadeElem.OuterOf[T]]
    val generatedCodecs =
      summonInstances[Tuple.Map[made.GeneratedElems, [Elem] =>> Elem match { case MadeElem.Of[elem] => elem }]](
        summonAllowed = true,
        deriveAllowed = false,
      ).toArrayOf[GenCodec[?]]

    inline made match {
      case made: Made.TransparentOf[T] =>
        deriveTransparentWrapper[T, made.MirroredElemType](
          compiletime.summonInline[GenCodec[made.MirroredElemType]],
          made.wrap,
          made.unwrap,
        )

      case made: Made.SingletonOf[T] =>
        compiletime.erasedValue[Tuple.Size[made.GeneratedElems]] match {
          case _: 0 => deriveSingleton(label, made.value)
          case _ => deriveSingletonWithGenerated(label, made.value, generatedNames, generatedExtractors, generatedCodecs)
        }

      case made: Made.ProductOf[T] =>
        deriveProduct(
          label,
          summonInstances[made.MirroredElemTypes](summonAllowed = true, deriveAllowed = false).toArrayOf[GenCodec[?]],
          made.mirroredElems.toArrayOf[MadeFieldElem].map(_.default),
          compiletime.constValueTuple[made.MirroredElemLabels].toArrayOf[String],
          made.fromUnsafeArray,
          made.mirroredElems.toArrayOf[MadeFieldElem],
          generatedNames,
          generatedExtractors,
          generatedCodecs,
        )

      case made: Made.SumOf[T] =>
        val instances =
          summonInstances[made.MirroredElemTypes](summonAllowed = false, deriveAllowed = true).toArrayOf[GenCodec[?]]
        val labels = compiletime.constValueTuple[made.MirroredElemLabels].toArrayOf[String]
        val classTags = compiletime.summonAll[Tuple.Map[made.MirroredElemTypes, ClassTag]].toArrayOf[ClassTag[?]]

        made.getAnnotation[flatten] match {
          case Some(f) =>
            deriveFlattenSum(
              label,
              instances,
              labels,
              f.caseFieldName,
              classTags,
              made.mirroredElems
                .toArrayOf[MadeFieldElem]
                .iterator
                .map(_.getAnnotation[defaultCase])
                .zipWithIndex
                .collectFirst { case (Some(default), i) => (i, default.transient) },
              compiletime.constValueTuple[made.MirroredElemLabels].toArrayOf[String].toSet,
            )
          case _ => deriveNestedSum(label, instances, labels, classTags)
        }
    }
  }

  inline private def summonInstances[Elems <: Tuple](
    summonAllowed: Boolean,
    deriveAllowed: Boolean,
  ): Tuple.Map[Elems, GenCodec] =
    inline compiletime.erasedValue[Elems] match {
      case _: (elem *: elems) =>
        val elemCodec = compiletime.summonFrom {
          case codec: GenCodec[`elem`] if summonAllowed => codec
          case _ if deriveAllowed => derived[elem]
          case _: AllowRecursiveDerivation.type => derived[elem]
        }
        (elemCodec *: summonInstances[elems](summonAllowed, deriveAllowed)).asInstanceOf[Tuple.Map[Elems, GenCodec]]
      case _: EmptyTuple => EmptyTuple.asInstanceOf[Tuple.Map[Elems, GenCodec]]
    }
  inline private def deriveTransparentWrapper[T, U](underlying: => GenCodec[U], unwrap: U => T, wrap: T => U)
    : GenCodec[T] = new TransformedCodec[T, U](underlying, wrap, unwrap)

  private def deriveSingleton[T](
    typeRepr: String,
    value: T,
  ): GenCodec[T] =
    new SingletonCodec[T & Singleton](typeRepr, value.asInstanceOf[T & Singleton]).asInstanceOf[GenCodec[T]]

  private def deriveSingletonWithGenerated[T](
    typeRepr: String,
    value: T,
    generatedNames: Array[String],
    generatedExtractors: Array[GeneratedMadeElem.OuterOf[T]],
    generatedCodecs: Array[GenCodec[?]],
  ): GenCodec[T] =
    new SingletonCodec[T & Singleton](typeRepr, value.asInstanceOf[T & Singleton]) {
      override def size(value: T & Singleton, output: Opt[SequentialOutput]): Int = generatedExtractors.size
      override def writeFields(output: ObjectOutput, value: T & Singleton): Unit =
        generatedExtractors.zipWithIndex.foreach { (extractor, index) =>
          writeField(
            generatedNames(index),
            output,
            extractor(value),
            generatedCodecs(index).asInstanceOf[GenCodec[extractor.MirroredType]],
          )
        }

    }.asInstanceOf[GenCodec[T]]

  private def deriveFlattenSum[T](
    typeRepr: String,
    instances: Array[GenCodec[?]],
    fieldNames: Array[String],
    caseFieldName: String,
    classes: Array[ClassTag[?]],
    defaultCase: Option[(idx: Int, transient: Boolean)],
    caseDependentFieldNames: Set[String],
  ): GenCodec[T] =
    new FlatSealedHierarchyCodec[T](
      typeRepr,
      fieldNames,
      classes.map(_.runtimeClass),
      Array.empty[String],
      caseDependentFieldNames,
      caseFieldName,
      defaultCase.map(_.idx).getOrElse(-1),
      defaultCase.exists(_.transient),
    ) {
      override def oooDependencies: Array[GenCodec[?]] = Array.empty
      override def caseDependencies: Array[OOOFieldsObjectCodec[?]] =
        instances.map(_.asInstanceOf[OOOFieldsObjectCodec[?]])
    }
  private def deriveNestedSum[T](
    typeRepr: String,
    instances: Array[GenCodec[?]],
    fieldNames: Array[String],
    classes: Array[ClassTag[?]],
  ): GenCodec[T] = new NestedSealedHierarchyCodec[T](
    typeRepr,
    fieldNames,
    classes.map(_.runtimeClass),
  ) {
    override def caseDependencies: Array[GenCodec[?]] = instances
  }
  private def deriveProduct[T](
    typeRepr: String,
    instances: Array[GenCodec[?]],
    defaults: Array[Option[Any]],
    fieldNames: Array[String],
    fromUnsafeArray: Array[Any] => T,
    fieldElems: Array[MadeFieldElem],
    generatedNames: Array[String],
    generatedExtractors: Array[GeneratedMadeElem.OuterOf[T]],
    generatedCodecs: Array[GenCodec[?]],
  ): GenCodec[T] =
    new ApplyUnapplyCodec[T](typeRepr, fieldNames) {

      private val transientDefaults = fieldElems.map(_.hasAnnotation[transientDefault])

      override protected val dependencies: Array[GenCodec[?]] = instances
      override protected def instantiate(fieldValues: FieldValues): T = ???
      override def size(value: T, output: Opt[SequentialOutput]): Int = ???
      override def writeFields(output: ObjectOutput, value: T): Unit = {
        ???
      }.asInstanceOf[GenCodec[T]]
    }
}
