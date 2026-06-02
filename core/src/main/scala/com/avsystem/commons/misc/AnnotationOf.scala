package com.avsystem.commons
package misc

import scala.annotation.{implicitNotFound, RefiningAnnotation}
import scala.quoted.{Expr, Quotes, Type}

/** A typeclass which captures an annotation of type `A` applied on a class/trait/object associated with type `T`. If
  * this annotation is absent, compilation will fail.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  */
@implicitNotFound("${T} is not annotated with ${A}")
case class AnnotationOf[A, T](annot: A) extends AnyVal
object AnnotationOf {
  inline given [A, T] => AnnotationOf[A, T] = ${ materializeAnnotationOfImpl[A, T] }

  private def materializeAnnotationOfImpl[A: Type, T: Type](using quotes: Quotes): Expr[AnnotationOf[A, T]] = {
    import quotes.reflect.*
    AnnotationOfMacros.annotsOfT[A, T].headOption match {
      case Some(annot) => '{ AnnotationOf[A, T](${ annot.asExprOf[A] }) }
      case None => report.errorAndAbort(s"${Type.show[T]} is not annotated with ${Type.show[A]}")
    }
  }
}

/** A typeclass which captures a possible annotation of type `A` applied on a class/trait/object associated with type
  * `T`. [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]]
  * apply.
  */
case class OptAnnotationOf[A, T](annotOpt: Opt[A])
object OptAnnotationOf {
  inline given [A, T] => OptAnnotationOf[A, T] = ${ materializeOptAnnotationOfImpl[A, T] }

  private def materializeOptAnnotationOfImpl[A: Type, T: Type](using quotes: Quotes): Expr[OptAnnotationOf[A, T]] = {
    val optExpr = AnnotationOfMacros.annotsOfT[A, T].headOption match {
      case Some(annot) => '{ Opt(${ annot.asExprOf[A] }) }
      case None => '{ Opt.Empty }
    }
    '{ OptAnnotationOf[A, T]($optExpr) }
  }
}

/** A typeclass which captures all annotations of type `A` applied on a class/trait/object associated with type `T`.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  */
case class AnnotationsOf[A, T](annots: List[A]) extends AnyVal
object AnnotationsOf {
  inline given [A, T] => AnnotationsOf[A, T] = ${ materializeAnnotationsOfImpl[A, T] }

  private def materializeAnnotationsOfImpl[A: Type, T: Type](using quotes: Quotes): Expr[AnnotationsOf[A, T]] = {
    val list = Expr.ofList(AnnotationOfMacros.annotsOfT[A, T].map(_.asExprOf[A]))
    '{ AnnotationsOf[A, T]($list) }
  }
}

/** A typeclass which serves as an evidence that an annotation of type `A` is applied on a class/trait/object associated
  * with type `T`.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  * Similar to [[AnnotationOf]] but does not reify the annotation itself into runtime.
  */
@implicitNotFound("${T} is not annotated with ${A}")
opaque type HasAnnotation[A <: RefiningAnnotation, T] = A

object HasAnnotation {
  transparent inline def check[A <: RefiningAnnotation, T]: Boolean = ${ checkImpl[A, T] }
  transparent inline def get[A <: RefiningAnnotation, T]: Option[A] = ${ getImpl[A, T] }

  private def checkImpl[A <: RefiningAnnotation: Type, T: Type](using quotes: Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[T].typeSymbol.hasAnnotation(TypeRepr.of[A].typeSymbol))
  }

  private def getImpl[A <: RefiningAnnotation: Type, T: Type](using quotes: Quotes): Expr[Option[A]] = {
    import quotes.reflect.*
    TypeRepr.of[T].typeSymbol.getAnnotation(TypeRepr.of[A].typeSymbol) match {
      case Some(annot) => '{ Some(${ annot.asExprOf[A] }) }
      case _ => Expr(None)
    }
  }
}

/** A typeclass which may be used in an implicit constructor parameter of an abstract class. Captures an annotation of
  * type `A` applied on a class or object which extends this abstract class. If this annotation is absent, compilation
  * will fail. [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]]
  * apply.
  *
  * @example
  *   {{{
  *   final class awesome(value: Boolean) extends scala.annotation.Annotation
  *
  *   abstract class Base(implicit awesomeAnnot: SelfAnnotation[awesome]) {
  *     def isAwesome: Boolean = awesomeAnnot.annot.value
  *   }
  *
  *   @awesome(true)
  *   class AwesomeSubclass extends Base
  *   }}}
  */
case class SelfAnnotation[A](annot: A) extends AnyVal
object SelfAnnotation {
  inline given [A] => SelfAnnotation[A] = ${ materializeSelfAnnotationImpl[A] }

  private def materializeSelfAnnotationImpl[A: Type](using quotes: Quotes): Expr[SelfAnnotation[A]] = {
    import quotes.reflect.*
    val sym = AnnotationOfMacros.enclosingClass
    if (sym == Symbol.noSymbol)
      report.errorAndAbort("SelfAnnotation must be used inside an enclosing class")
    AnnotationOfMacros.annotsOfSym[A](sym).headOption match {
      case Some(annot) => '{ SelfAnnotation[A](${ annot.asExprOf[A] }) }
      case None => report.errorAndAbort(s"No annotation of type ${Type.show[A]} on enclosing class ${sym.name}")
    }
  }
}

/** A typeclass which may be used in an implicit constructor parameter of an abstract class. Captures a possible
  * annotation of type `A` applied on a class or object which extends this abstract class.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  *
  * @example
  *   {{{
  *   final class awesome(value: Boolean) extends scala.annotation.Annotation
  *
  *   abstract class Base(implicit awesomeAnnot: SelfOptAnnotation[awesome]) {
  *     def isAwesome: Boolean = awesomeAnnot.annotOpt.exists(_.value)
  *   }
  *
  *   class NotAwesomeSubclass extends Base
  *   @awesome(true) class AwesomeSubclass extends Base
  *   @awesome(false) class ExplicitlyNotAwesomeSubclass extends Base
  *   }}}
  */
case class SelfOptAnnotation[A](annotOpt: Opt[A])
object SelfOptAnnotation {
  inline given [A] => SelfOptAnnotation[A] = ${ materializeSelfOptAnnotationImpl[A] }

  private def materializeSelfOptAnnotationImpl[A: Type](using quotes: Quotes): Expr[SelfOptAnnotation[A]] = {
    import quotes.reflect.*
    val sym = AnnotationOfMacros.enclosingClass
    if (sym == Symbol.noSymbol)
      report.errorAndAbort("SelfOptAnnotation must be used inside an enclosing class")
    val optExpr = AnnotationOfMacros.annotsOfSym[A](sym).headOption match {
      case Some(annot) => '{ Opt(${ annot.asExprOf[A] }) }
      case None => '{ Opt.Empty }
    }
    '{ SelfOptAnnotation[A]($optExpr) }
  }
}

/** A typeclass which may be used in an implicit constructor parameter of an abstract class. Captures all annotations of
  * type `A` applied on a class or object which extends this abstract class.
  * [[https://github.com/AVSystem/scala-commons/blob/master/docs/Annotations.md Annotation processing rules]] apply.
  *
  * @example
  *   {{{
  *   final class tag(value: String) extends scala.annotation.Annotation
  *
  *   abstract class Base(implicit tagAnnots: SelfAnnotations[tag]) {
  *     def tags: List[String] = tagAnnots.map(_.value)
  *   }
  *
  *   @tag("t1") @tag("t2") @tag("t3")
  *   class TaggedSubclass extends Base
  *   }}}
  */
case class SelfAnnotations[A](annots: List[A]) extends AnyVal
object SelfAnnotations {
  inline given [A] => SelfAnnotations[A] = ${ materializeSelfAnnotationsImpl[A] }

  private def materializeSelfAnnotationsImpl[A: Type](using quotes: Quotes): Expr[SelfAnnotations[A]] = {
    import quotes.reflect.*
    val sym = AnnotationOfMacros.enclosingClass
    if (sym == Symbol.noSymbol)
      report.errorAndAbort("SelfAnnotations must be used inside an enclosing class")
    val list = Expr.ofList(AnnotationOfMacros.annotsOfSym[A](sym).map(_.asExprOf[A]))
    '{ SelfAnnotations[A]($list) }
  }
}

/** Private helper object — shared scala-3 reflection plumbing for the AnnotationOf family (annotation lookup +
  * `AnnotationAggregate` expansion). Cribbed verbatim per-method from
  * `origin/master:core/src/main/scala-3/com/avsystem/commons/misc/MiscMacros.scala`. Kept file-local per feedback (no
  * central `MiscMacros.scala` bundle).
  */
private object AnnotationOfMacros {

  def annotsOfT[A: Type, T: Type](using quotes: Quotes): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    val aSym = TypeRepr.of[A].typeSymbol
    expandAggregates(TypeRepr.of[T].dealias.typeSymbol.annotations).filter(_.tpe.typeSymbol == aSym)
  }

  def annotsOfSym[A: Type](using quotes: Quotes)(sym: quotes.reflect.Symbol): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    val aSym = TypeRepr.of[A].typeSymbol
    expandAggregates(sym.annotations).filter(_.tpe.typeSymbol == aSym)
  }

  def enclosingClass(using quotes: Quotes): quotes.reflect.Symbol = {
    import quotes.reflect.*
    var sym = Symbol.spliceOwner
    while (sym != Symbol.noSymbol && !sym.isClassDef) sym = sym.owner
    sym
  }

  /** Recursively replace any `AnnotationAggregate` annotations with the annotations declared on the aggregate's
    * `aggregated` method, substituting references to the aggregate's constructor parameters with the actual arguments.
    */
  def expandAggregates(using quotes: Quotes)(annots: List[quotes.reflect.Term]): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    val aggregateTpe = TypeRepr.of[com.avsystem.commons.annotation.AnnotationAggregate]

    def expand(annot: Term): List[Term] =
      if (!(annot.tpe <:< aggregateTpe)) List(annot)
      else {
        val cls = annot.tpe.typeSymbol
        cls.declaredMethods.find(_.name == "aggregated") match {
          case None => List(annot)
          case Some(aggMethod) =>
            val classTypeParams: List[Symbol] = cls.declaredTypes.filter(_.isTypeParam)
            val valueParams = cls.primaryConstructor.paramSymss.flatten.filterNot(_.isType)
            val (outerTypeArgs, outerValueArgs) = collectArgs(annot)
            val typeMap: List[(Symbol, TypeRepr)] = classTypeParams.zip(outerTypeArgs)
            val valueMap: Map[Symbol, Term] = valueParams.zip(outerValueArgs).toMap
            val rawInner = aggMethod.annotations.filter(_.tpe <:< TypeRepr.of[scala.annotation.StaticAnnotation])
            rawInner.flatMap(inner => expand(rebuildAnnot(inner, typeMap, valueMap)))
        }
      }

    annots.reverse.flatMap(expand)
  }

  private def collectArgs(using quotes: Quotes)(annot: quotes.reflect.Term)
    : (List[quotes.reflect.TypeRepr], List[quotes.reflect.Term]) = {
    import quotes.reflect.*
    def loop(t: Term, vAcc: List[Term]): (List[TypeRepr], List[Term]) = t match {
      case Apply(fun, args) => loop(fun, args ++ vAcc)
      case TypeApply(fun, tArgs) =>
        val (_, vs) = loop(fun, vAcc)
        (tArgs.map(_.tpe), vs)
      case Select(New(tpt), _) => (tpt.tpe.typeArgs, vAcc)
      case _ => (Nil, vAcc)
    }
    loop(annot, Nil)
  }

  private def rebuildAnnot(
    using quotes: Quotes
  )(
    inner: quotes.reflect.Term,
    typeMap: List[(quotes.reflect.Symbol, quotes.reflect.TypeRepr)],
    valueMap: Map[quotes.reflect.Symbol, quotes.reflect.Term],
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    val (typeKeys, typeVals) = typeMap.unzip
    val annotCls = inner.tpe.typeSymbol
    val concreteAnnotTpe = inner.tpe.substituteTypes(typeKeys, typeVals)
    val ctor = annotCls.primaryConstructor

    val rawArgs = collectArgs(inner)._2
    val concreteArgs = rawArgs.map(substituteRefs(_, valueMap, typeKeys, typeVals))

    val classTpe = annotCls.typeRef
    val newTree: Term = New(Inferred(classTpe))
    val selectedCtor: Term = Select(newTree, ctor)
    val typeArgs = concreteAnnotTpe.typeArgs
    val withTypeArgs: Term =
      if (typeArgs.isEmpty) selectedCtor
      else TypeApply(selectedCtor, typeArgs.map(t => Inferred(t)))
    Apply(withTypeArgs, concreteArgs)
  }

  private def substituteRefs(
    using quotes: Quotes
  )(
    term: quotes.reflect.Term,
    valueMap: Map[quotes.reflect.Symbol, quotes.reflect.Term],
    typeKeys: List[quotes.reflect.Symbol],
    typeVals: List[quotes.reflect.TypeRepr],
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    val byName: Map[String, Term] = valueMap.map { case (sym, t) => sym.name -> t }
    val mapper = new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
        case id: Ident if byName.contains(id.name) => byName(id.name)
        case Select(This(_), name) if byName.contains(name) => byName(name)
        case _ => super.transformTerm(tree)(owner)
      }
    }
    mapper.transformTerm(term)(Symbol.spliceOwner)
  }
}
