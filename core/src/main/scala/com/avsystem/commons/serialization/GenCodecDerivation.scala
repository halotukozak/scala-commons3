package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.*
import made.*
trait GenCodecDerivation { this: GenCodec.type =>
  private inline given containsOnlyRefl[Tup <: Tuple, T]: (Tup containsOnly T) = containsOnly.refl
  inline def derived[T]: GenCodec[T] = {
    given deferred: DeferredCodec[T] = new DeferredCodec[T]
    val underlying = unsafeDerived[T](using compiletime.summonInline[Made.Of[T]])
    deferred.underlying = underlying
    underlying
  }

  inline def deriveRecursively[T]: GenCodec[T] = {
    given AllowRecursiveDerivation.type = AllowRecursiveDerivation
    derived[T]
  }

  inline private def unsafeDerived[T: Made.Of as made]: GenCodec[T] = {
    val label = compiletime.constValue[made.Label]
    val generatedNames = compiletime
      .constValueTuple[Tuple.Map[made.GeneratedElems, MadeElem.ExtractLabel]]
      .toArrayOf[String]
    val generatedExtractors = made.generatedElems.toArrayOf[GeneratedMadeElem.OuterOf[T]]
    val generatedCodecs =
      summonInstances[Tuple.Map[made.GeneratedElems, MadeElem.ExtractOf]](
        summonAllowed = true,
        deriveAllowed = false,
      ).toArrayOf[GenCodec[?]]

    inline made match {
      case made: Made.TransparentOf[T] =>
        deriveTransparentWrapper[T, made.ElemType](
          compiletime.summonInline[GenCodec[made.ElemType]],
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
          summonInstances[made.ElemTypes](summonAllowed = true, deriveAllowed = false).toArrayOf[GenCodec[?]],
          made.elems.toArrayOf[MadeFieldElem].map(_.default),
          compiletime.constValueTuple[made.ElemLabels].toArrayOf[String],
          made.fromUnsafeArray,
          made.elems.toArrayOf[MadeFieldElem],
          generatedNames,
          generatedExtractors,
          generatedCodecs,
        )

      case made: Made.SumOf[T] =>
        val instances =
          summonInstances[made.ElemTypes](summonAllowed = false, deriveAllowed = true).toArrayOf[GenCodec[?]]
        val labels = compiletime.constValueTuple[made.ElemLabels].toArrayOf[String]
        val classTags = compiletime.summonAll[Tuple.Map[made.ElemTypes, ClassTag]].toArrayOf[ClassTag[?]]

        made.getAnnotation[flatten] match {
          case Some(f) =>
            deriveFlattenSum(
              label,
              instances,
              labels,
              f.caseFieldName,
              classTags,
              made.elems
                .toArrayOf[MadeFieldElem]
                .iterator
                .map(_.getAnnotation[defaultCase])
                .zipWithIndex
                .collectFirst { case (Some(default), i) => (i, default.transient) },
              compiletime.constValueTuple[made.ElemLabels].toArrayOf[String].toSet,
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
            generatedCodecs(index).asInstanceOf[GenCodec[extractor.Type]],
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

      private val transientDefaults: Array[Boolean] = fieldElems.map(_.hasAnnotation[transientDefault])

      override protected val dependencies: Array[GenCodec[?]] = instances

      override protected def instantiate(fieldValues: FieldValues): T = {
        val values = new Array[Any](fieldNames.length)
        var i = 0
        while (i < values.length) {
          values(i) = defaults(i) match {
            case Some(d) => fieldValues.getOrElse[Any](i, d)
            case None => getField[Any](fieldValues, i)
          }
          i += 1
        }
        fromUnsafeArray(values)
      }

      private def isTransient(idx: Int, value: Any): Boolean =
        transientDefaults(idx) && defaults(idx).contains(value)

      override def size(value: T, output: Opt[SequentialOutput]): Int = {
        val product = value.asInstanceOf[Product]
        var count = generatedExtractors.length
        var i = 0
        while (i < fieldNames.length) {
          if (!isTransient(i, product.productElement(i))) count += 1
          i += 1
        }
        count
      }

      override def writeFields(output: ObjectOutput, value: T): Unit = {
        val product = value.asInstanceOf[Product]
        var i = 0
        while (i < fieldNames.length) {
          val v = product.productElement(i)
          if (!isTransient(i, v)) {
            writeField[Any](output, i, v)
          }
          i += 1
        }
        var j = 0
        while (j < generatedExtractors.length) {
          val extractor = generatedExtractors(j)
          writeField[Any](
            generatedNames(j),
            output,
            extractor(value),
            generatedCodecs(j).asInstanceOf[GenCodec[Any]],
          )
          j += 1
        }
      }
    }
}
