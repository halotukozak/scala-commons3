package com.avsystem.commons
package misc

import com.avsystem.commons.annotation.TodoScala3Migration

import scala.quoted.*

trait AnnotationOfMacros {
  inline given [A, T] => AnnotationOf[A, T] = ${ MiscMacros.materializeAnnotationOf[A, T] }
}
trait OptAnnotationOfMacros {
  inline given [A, T] => OptAnnotationOf[A, T] = ${ MiscMacros.materializeOptAnnotationOf[A, T] }
}
trait AnnotationsOfMacros {
  inline given [A, T] => AnnotationsOf[A, T] = ${ MiscMacros.materializeAnnotationsOf[A, T] }
}
trait SelfAnnotationMacros {
  inline given [A] => SelfAnnotation[A] = ${ MiscMacros.materializeSelfAnnotation[A] }
}
trait SelfOptAnnotationMacros {
  inline given [A] => SelfOptAnnotation[A] = ${ MiscMacros.materializeSelfOptAnnotation[A] }
}
trait SelfAnnotationsMacros {
  inline given [A] => SelfAnnotations[A] = ${ MiscMacros.materializeSelfAnnotations[A] }
}

trait SimpleClassNameMacros {
  inline given [T] => SimpleClassName[T] = ${ MiscMacros.materializeSimpleClassName[T] }
}

trait SourceInfoMacros {
  inline given SourceInfo = ${ MiscMacros.materializeSourceInfo }
}

@TodoScala3Migration(
  "Implicits.infer family — need real implicit-search quoted impl, otherwise @implicitNotFound never fires",
)
trait ImplicitsMacros {
  inline def infer[T]: T = ${ MiscMacros.inferImpl[T] }
  inline def infer[T](inline clue: String): T = ${ MiscMacros.clueInferImpl[T]('clue) }
  inline def inferNonMacro[T](inline clue: String): T = ${ MiscMacros.inferNonMacroImpl[T]('clue) }
}

@TodoScala3Migration("SelfInstance.materialize is a stub")
trait SelfInstanceMacros {
  inline given [C[_]] => SelfInstance[C] = ???
}

@TodoScala3Migration("Delegation.materializeDelegation is a stub — DelegationTest is `ignore`d")
trait DelegationMacros {
  inline given [A, B] => Delegation[A, B] = ???
}
@TodoScala3Migration("Delegation.apply is a stub — DelegationTest is `ignore`d")
trait DelegationApplyMacros[B] {
  inline def apply[A](inline source: A): B = ???
}

object MiscMacros {

  /**
   * Scala-3 port of the scala-2 `optionalizeFirstArg` macro. Rewrites `f.call(arg, more*)` into
   * `if (arg ne null) f.call(arg, more*) else f.call(more*)` — used by mongo driver wrappers to
   * skip an optional first session argument when it's null.
   */
  inline def optionalizeFirstArg[T](inline expr: T): T = ${ optionalizeFirstArgImpl[T]('expr) }

  def optionalizeFirstArgImpl[T: Type](expr: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*
    def go(t: Term): Term = t match {
      case t @ Apply(fun, head :: tail) =>
        val (receiver, methodName, targs) = fun match {
          case Select(recv, name) => (recv, name, Nil)
          case TypeApply(Select(recv, name), ts) => (recv, name, ts.map(_.tpe))
          case other =>
            report.errorAndAbort(
              s"optionalizeFirstArg: expected Select for method, got ${other.show}",
              other.pos,
            )
        }
        val condExpr = Select
          .unique(Select.unique(head, "asInstanceOf").appliedToType(TypeRepr.of[Object]), "ne")
          .appliedTo('{ null }.asTerm)
        val fallback = Select.overloaded(receiver, methodName, targs, tail)
        If(condExpr, t, fallback).asExprOf[T].asTerm
      case TypeApply(inner, targs) =>
        // strip type-args, recurse, re-apply
        go(inner) match {
          case ifExpr: If => ifExpr // already complete
          case other => TypeApply(other, targs)
        }
      case Block(stats, expr) => Block(stats, go(expr))
      case Inlined(_, Nil, expr) => go(expr)
      case Typed(expr, _) => go(expr)
      case _ =>
        report.errorAndAbort(
          s"optionalizeFirstArg: function application expected, got ${t.show}",
          t.pos,
        )
    }
    go(expr.asTerm).asExprOf[T]
  }

  private def annotsOfT[A: Type, T: Type](using quotes: Quotes): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    val aSym = TypeRepr.of[A].typeSymbol
    expandAggregates(TypeRepr.of[T].dealias.typeSymbol.annotations).filter(_.tpe.typeSymbol == aSym)
  }

  /**
   * Recursively replace any `AnnotationAggregate` annotations with the annotations declared on the aggregate's
   * `aggregated` method, substituting references to the aggregate's constructor parameters with the actual arguments.
   */
  private def expandAggregates(using quotes: Quotes)(annots: List[quotes.reflect.Term]): List[quotes.reflect.Term] = {
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
    using quotes: Quotes,
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
    using quotes: Quotes,
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

  private def enclosingClass(using quotes: Quotes): quotes.reflect.Symbol = {
    import quotes.reflect.*
    var sym = Symbol.spliceOwner
    while (sym != Symbol.noSymbol && !sym.isClassDef) sym = sym.owner
    sym
  }

  private def annotsOfSym[A: Type](using quotes: Quotes)(sym: quotes.reflect.Symbol): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    val aSym = TypeRepr.of[A].typeSymbol
    expandAggregates(sym.annotations).filter(_.tpe.typeSymbol == aSym)
  }

  def materializeSelfAnnotation[A: Type](using quotes: Quotes): Expr[SelfAnnotation[A]] = {
    import quotes.reflect.*
    val sym = enclosingClass
    if (sym == Symbol.noSymbol)
      report.errorAndAbort("SelfAnnotation must be used inside an enclosing class")
    annotsOfSym[A](sym).headOption match {
      case Some(annot) => '{ SelfAnnotation[A](${ annot.asExprOf[A] }) }
      case None => report.errorAndAbort(s"No annotation of type ${Type.show[A]} on enclosing class ${sym.name}")
    }
  }

  def materializeSelfOptAnnotation[A: Type](using quotes: Quotes): Expr[SelfOptAnnotation[A]] = {
    import quotes.reflect.*
    val sym = enclosingClass
    if (sym == Symbol.noSymbol)
      report.errorAndAbort("SelfOptAnnotation must be used inside an enclosing class")
    val optExpr = annotsOfSym[A](sym).headOption match {
      case Some(annot) => '{ Opt(${ annot.asExprOf[A] }) }
      case None => '{ Opt.Empty }
    }
    '{ SelfOptAnnotation[A]($optExpr) }
  }

  def materializeSelfAnnotations[A: Type](using quotes: Quotes): Expr[SelfAnnotations[A]] = {
    import quotes.reflect.*
    val sym = enclosingClass
    if (sym == Symbol.noSymbol)
      report.errorAndAbort("SelfAnnotations must be used inside an enclosing class")
    val list = Expr.ofList(annotsOfSym[A](sym).map(_.asExprOf[A]))
    '{ SelfAnnotations[A]($list) }
  }
  def materializeSimpleClassName[T: Type](using quotes: Quotes): Expr[SimpleClassName[T]] = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].dealias.typeSymbol
    val name = Expr(sym.name.stripSuffix("$"))
    '{ SimpleClassName[T]($name) }
  }
  def materializeSourceInfo(using Quotes): Expr[SourceInfo] = SourceInfo.hereImpl
  def inferImpl[T: Type](using Quotes): Expr[T] = inferTpe[T]("")
  def clueInferImpl[T: Type](clue: Expr[String])(using Quotes): Expr[T] =
    inferTpe[T](clue.valueOrAbort)
  def inferNonMacroImpl[T: Type](clue: Expr[String])(using Quotes): Expr[T] =
    // The Scala 2 macro disables further macro expansion for this search; Scala 3's Implicits.search
    // doesn't expose that knob, so we fall back to the same path as `infer`.
    inferTpe[T](clue.valueOrAbort)

  private def inferTpe[T: Type](clue: String)(using quotes: Quotes): Expr[T] = {
    import quotes.reflect.*
    Implicits.search(TypeRepr.of[T]) match {
      case s: ImplicitSearchSuccess => s.tree.asExprOf[T]
      case _: ImplicitSearchFailure =>
        // Mimic upstream `implicitNotFoundMsg`: look up @implicitNotFound message via
        // ImplicitNotFound[T] sentinel — if the user provided one, use its message; otherwise
        // fall back to the default failure explanation.
        val msg = implicitNotFoundMessage[T].getOrElse(s"no implicit value for ${Type.show[T]}")
        report.errorAndAbort((if (clue.isEmpty) "" else clue + "\n") + msg)
    }
  }

  private def implicitNotFoundMessage[T: Type](using quotes: Quotes): Option[String] = {
    import quotes.reflect.*
    val implicitNotFoundCls = Symbol.requiredClass("scala.annotation.implicitNotFound")
    val notFoundType = TypeRepr.of[ImplicitNotFound[T]]
    Implicits.search(notFoundType) match {
      case s: ImplicitSearchSuccess =>
        val annots = s.tree.symbol.annotations ++ s.tree.tpe.typeSymbol.annotations
        annots.collectFirst {
          case a if a.tpe.typeSymbol == implicitNotFoundCls =>
            a match {
              case Apply(_, args) =>
                args.collectFirst { case Literal(StringConstant(msg)) => msg }
              case _ => None
            }
        }.flatten
      case _ => None
    }
  }
}
