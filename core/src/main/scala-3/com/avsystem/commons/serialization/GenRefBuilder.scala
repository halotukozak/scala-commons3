package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.{Opt, OptArg, OptRef}

import scala.annotation.tailrec
import scala.quoted.*

private[serialization] object GenRefBuilder {

  def buildRawRef[S: Type, T: Type](fun: Expr[S => T])(using quotes: Quotes): Expr[RawRef] = {
    import quotes.reflect.*

    val transparentGetSyms: Set[Symbol] = Set(
      TypeRepr.of[Opt[Any]].typeSymbol,
      TypeRepr.of[OptArg[Any]].typeSymbol,
      TypeRepr.of[OptRef[Any]].typeSymbol,
    ).flatMap(_.methodMember("get"))

    val flattenSym = TypeRepr.of[flatten].typeSymbol
    val transparentSym = TypeRepr.of[made.annotation.transparent].typeSymbol
    val generatedSym = TypeRepr.of[made.annotation.generated].typeSymbol
    val nameSym = TypeRepr.of[made.annotation.name].typeSymbol

    def targetName(sym: Symbol): String = sym.getAnnotation(nameSym) match {
      case Some(Apply(_, List(Literal(StringConstant(s))))) => s
      case _ => sym.name
    }

    def isTransparentWrapper(prefixTpe: TypeRepr, fieldSym: Symbol): Boolean = {
      val typeSym = prefixTpe.typeSymbol
      if (!typeSym.flags.is(Flags.Case)) false
      else
        typeSym.primaryConstructor.paramSymss.flatten.filterNot(_.isType) match {
          case List(only) if only.name == fieldSym.name =>
            typeSym.hasAnnotation(transparentSym) || {
              val paramTpe = prefixTpe.memberType(only).widen
              val wrappingTpe = TypeRepr.of[made.TransparentWrapping].appliedTo(List(paramTpe, prefixTpe))
              Implicits.search(wrappingTpe) match {
                case _: ImplicitSearchSuccess => true
                case _ => false
              }
            }
          case _ => false
        }
    }

    val term = fun.asTerm
    val (paramSym, bodyTerm) = extractLambda(term)

    val refs = scala.collection.mutable.ListBuffer.empty[Expr[RawRef]]

    @tailrec def extract(body: Term): Unit = body match {
      case Inlined(_, _, inner) => extract(inner)
      case Block(Nil, expr) => extract(expr)
      case Typed(prefix, _) => extract(prefix)

      case Select(prefix, _) if transparentGetSyms.contains(body.symbol) =>
        extract(prefix)

      case sel @ Select(prefix, _) =>
        val prefixTpe = prefix.tpe.widen
        val selSym = sel.symbol

        def fieldMemberFor(tpe: TypeRepr, member: Symbol): Symbol =
          if (member.hasAnnotation(generatedSym)) member
          else if (member.flags.is(Flags.CaseAccessor) || member.flags.is(Flags.ParamAccessor)) {
            val ctor = tpe.typeSymbol.primaryConstructor
            ctor.paramSymss.flatten.find(_.name == member.name).getOrElse(member)
          } else
            report.errorAndAbort(
              s"$member in $prefixTpe is neither a case class field accessor nor a @generated member",
              sel.pos,
            )

        val isSealedFlatten =
          prefixTpe.typeSymbol.flags.is(Flags.Sealed) && prefixTpe.typeSymbol.hasAnnotation(flattenSym)

        if (isSealedFlatten) {
          val subtypes = prefixTpe.typeSymbol.children
          val (memberName, fieldType) = subtypes.headOption match {
            case None =>
              report.errorAndAbort(s"$prefixTpe is sealed but has no known subtypes", sel.pos)
            case Some(_) =>
              val perSub = subtypes.map { subSym =>
                val subTpe = subSym.typeRef
                val sub =
                  subTpe.typeSymbol.methodMember(selSym.name).find(s => s == selSym || s.allOverriddenSymbols.contains(selSym))
                    .orElse(subTpe.typeSymbol.fieldMember(selSym.name) match {
                      case s if s.exists => Some(s)
                      case _ => None
                    })
                    .getOrElse(
                      report.errorAndAbort(s"No overriding member for ${selSym.name} in $subTpe", sel.pos),
                    )
                val field = fieldMemberFor(subTpe, sub)
                (targetName(field), subTpe.memberType(field).widen)
              }
              val (firstName, firstTpe) = perSub.head
              perSub.tail.foreach { case (n, t) =>
                if (n != firstName)
                  report.errorAndAbort(
                    s"All members overriding ${selSym.name} in subtypes must share @name",
                    sel.pos,
                  )
              }
              (firstName, firstTpe)
          }
          refs.prepend('{ RawRef.Field(${ Expr(memberName) }) })
        } else {
          val fieldSym = fieldMemberFor(prefixTpe, selSym)
          if (!isTransparentWrapper(prefixTpe, fieldSym))
            refs.prepend('{ RawRef.Field(${ Expr(targetName(fieldSym)) }) })
        }
        extract(prefix)

      case Apply(Select(prefix, "apply"), List(arg)) if isMapApply(prefix) =>
        emitKeyRef(prefix, arg)
        extract(prefix)

      case Apply(Select(prefix, "get"), List(arg)) if isJMapGet(prefix) =>
        emitKeyRef(prefix, arg)
        extract(prefix)

      case Apply(prefix, Nil) => extract(prefix)

      case id: Ident if id.symbol == paramSym => ()

      case other =>
        report.errorAndAbort(s"This invocation can't be translated into RawRef: ${other.show}", other.pos)
    }

    def isMapApply(prefix: Term)(using Quotes): Boolean =
      prefix.tpe.baseClasses.exists(_ == TypeRepr.of[scala.collection.Map[Any, Any]].typeSymbol)

    def isJMapGet(prefix: Term)(using Quotes): Boolean =
      prefix.tpe.baseClasses.exists(_ == TypeRepr.of[java.util.Map[Any, Any]].typeSymbol)

    def emitKeyRef(prefix: Term, arg: Term): Unit = {
      val mapBase =
        if (isMapApply(prefix)) TypeRepr.of[scala.collection.Map[Any, Any]].typeSymbol
        else TypeRepr.of[java.util.Map[Any, Any]].typeSymbol
      val keyTpe = prefix.tpe.baseType(mapBase).typeArgs.head
      keyTpe.asType match {
        case '[k] =>
          val argExpr = arg.asExprOf[k]
          refs.prepend('{ RawRef.Field(GenKeyCodec.write[k]($argExpr)(using compiletime.summonInline[GenKeyCodec[k]])) })
      }
    }

    extract(bodyTerm)

    refs.toList match {
      case Nil => '{ RawRef.Identity }
      case one :: Nil => one
      case many => many.reduce((a, b) => '{ RawRef.Composite($a, $b) })
    }
  }

  /** Unwrap `fun` to extract the lambda's parameter symbol and body Term. */
  private def extractLambda(using quotes: Quotes)(t: quotes.reflect.Term): (quotes.reflect.Symbol, quotes.reflect.Term) = {
    import quotes.reflect.*
    def loop(tree: Term): (Symbol, Term) = tree match {
      case Inlined(_, _, inner) => loop(inner)
      case Block(Nil, expr) => loop(expr)
      case Lambda(List(p), body) => (p.symbol, body)
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(body))), _: Closure) =>
        (p.symbol, body)
      case other =>
        report.errorAndAbort(s"Expected lambda expression, got: ${other.show}", other.pos)
    }
    loop(t)
  }
}
