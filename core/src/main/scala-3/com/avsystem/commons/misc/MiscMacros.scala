package com.avsystem.commons
package misc

import com.avsystem.commons.annotation.TodoScala3Migration

import scala.quoted.*

trait AnnotationOfMacros {
  inline implicit def materialize[A, T]: AnnotationOf[A, T] = ${ MiscMacros.materializeAnnotationOf[A, T] }
}
trait OptAnnotationOfMacros {
  inline implicit def materialize[A, T]: OptAnnotationOf[A, T] = ${ MiscMacros.materializeOptAnnotationOf[A, T] }
}
trait AnnotationsOfMacros {
  inline implicit def materialize[A, T]: AnnotationsOf[A, T] = ${ MiscMacros.materializeAnnotationsOf[A, T] }
}
trait SelfAnnotationMacros {
  inline implicit def materialize[A]: SelfAnnotation[A] = ${ MiscMacros.materializeSelfAnnotation[A] }
}
trait SelfOptAnnotationMacros {
  inline implicit def materialize[A]: SelfOptAnnotation[A] = ${ MiscMacros.materializeSelfOptAnnotation[A] }
}
trait SelfAnnotationsMacros {
  inline implicit def materialize[A]: SelfAnnotations[A] = ${ MiscMacros.materializeSelfAnnotations[A] }
}

trait SimpleClassNameMacros {
  inline implicit def materialize[T]: SimpleClassName[T] = ${ MiscMacros.materializeSimpleClassName[T] }
}

trait SourceInfoMacros {
  inline implicit def here: SourceInfo = ${ MiscMacros.materializeSourceInfo }
}

@TodoScala3Migration("Implicits.infer family — need real implicit-search quoted impl, otherwise @implicitNotFound never fires")
trait ImplicitsMacros {
  inline def infer[T]: T = ${ MiscMacros.inferImpl[T] }
  inline def infer[T](inline clue: String): T = ${ MiscMacros.clueInferImpl[T]('clue) }
  inline def inferNonMacro[T](inline clue: String): T = ${ MiscMacros.inferNonMacroImpl[T]('clue) }
}

@TodoScala3Migration("SelfInstance.materialize is a stub")
trait SelfInstanceMacros {
  inline implicit def materialize[C[_]]: SelfInstance[C] = ???
}

@TodoScala3Migration("Delegation.materializeDelegation is a stub — DelegationTest is `ignore`d")
trait DelegationMacros {
  inline implicit def materializeDelegation[A, B]: Delegation[A, B] = ???
}
@TodoScala3Migration("Delegation.apply is a stub — DelegationTest is `ignore`d")
trait DelegationApplyMacros[B] {
  inline def apply[A](inline source: A): B = ???
}

object MiscMacros {
  private def annotsOfT[A: Type, T: Type](using quotes: Quotes): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    val aSym = TypeRepr.of[A].typeSymbol
    TypeRepr.of[T].dealias.typeSymbol.annotations.filter(_.tpe.typeSymbol == aSym)
  }

  def materializeAnnotationOf[A: Type, T: Type](using quotes: Quotes): Expr[AnnotationOf[A, T]] = {
    import quotes.reflect.*
    annotsOfT[A, T].headOption match {
      case Some(annot) => '{ AnnotationOf[A, T](${ annot.asExprOf[A] }) }
      case None => report.errorAndAbort(s"${Type.show[T]} is not annotated with ${Type.show[A]}")
    }
  }

  def materializeOptAnnotationOf[A: Type, T: Type](using quotes: Quotes): Expr[OptAnnotationOf[A, T]] = {
    import quotes.reflect.*
    val optExpr = annotsOfT[A, T].headOption match {
      case Some(annot) => '{ Opt(${ annot.asExprOf[A] }) }
      case None => '{ Opt.Empty }
    }
    '{ OptAnnotationOf[A, T]($optExpr) }
  }

  def materializeAnnotationsOf[A: Type, T: Type](using quotes: Quotes): Expr[AnnotationsOf[A, T]] = {
    import quotes.reflect.*
    val list = Expr.ofList(annotsOfT[A, T].map(_.asExprOf[A]))
    '{ AnnotationsOf[A, T]($list) }
  }

  @TodoScala3Migration("SelfAnnotation family: needs enclosing class lookup like Scala 2 c.internal.enclosingOwner")
  def materializeSelfAnnotation[A: Type](using Quotes): Expr[SelfAnnotation[A]] = '{ ??? }
  @TodoScala3Migration("SelfOptAnnotation family: needs enclosing class lookup")
  def materializeSelfOptAnnotation[A: Type](using Quotes): Expr[SelfOptAnnotation[A]] = '{ ??? }
  @TodoScala3Migration("SelfAnnotations family: needs enclosing class lookup")
  def materializeSelfAnnotations[A: Type](using Quotes): Expr[SelfAnnotations[A]] = '{ ??? }
  def materializeSimpleClassName[T: Type](using quotes: Quotes): Expr[SimpleClassName[T]] = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].dealias.typeSymbol
    val name = Expr(sym.name.stripSuffix("$"))
    '{ SimpleClassName[T]($name) }
  }
  def materializeSourceInfo(using Quotes): Expr[SourceInfo] = '{ ??? }
  def inferImpl[T: Type](using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
  def clueInferImpl[T: Type](clue: Expr[String])(using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
  def inferNonMacroImpl[T: Type](clue: Expr[String])(using Quotes): Expr[T] = '{ ??? }.asInstanceOf[Expr[T]]
}
