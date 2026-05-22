package com.avsystem.commons
package rpc

import made.Done

import scala.quoted.*

/**
 * Macro-driven derivation of RPC type-classes (`AsRaw`, `AsReal`) using `made.Done` as a
 * compile-time mirror of the Real RPC trait.
 *
 * **Tier 1 (current)** — "echo proxy": Raw and Real are traits with matching method names
 * and parameter counts. `asRaw(real)` returns a `Raw` whose methods translate each argument
 * via `AsReal[RawArg, RealArg]` and translate the return value via `AsRaw[RawRet, RealRet]`.
 *
 * Out of scope for Tier 1 (reported via `report.errorAndAbort`):
 *   - `@multi` / `@verbatim` / `@encoded` / `@composite` / `@methodName` / `@annotated`
 *   - overloaded methods (use `@rpcName` to disambiguate)
 *   - method type parameters (`@forTypeParams`, `@infer`)
 *   - implicit parameter lists
 *   - getter chains (methods returning another RPC trait)
 *
 * All Tier-2+ work is tracked via `@TodoScala3Migration` markers.
 */
private[rpc] object RpcDerivation:

  inline def materializeAsRaw[Raw, Real]: AsRaw[Raw, Real] = ${ materializeAsRawImpl[Raw, Real] }
  inline def materializeAsReal[Raw, Real]: AsReal[Raw, Real] = ${ materializeAsRealImpl[Raw, Real] }

  /** Diagnostic helper — summons `Done.Of[T]` so call sites can inspect a Real trait. */
  inline def doneOf[T]: Done.Of[T] = scala.compiletime.summonInline[Done.Of[T]]

  private def materializeAsRawImpl[Raw: Type, Real: Type](using Quotes): Expr[AsRaw[Raw, Real]] =
    import quotes.reflect.*
    val realTpe = TypeRepr.of[Real]
    val rawTpe = TypeRepr.of[Raw]
    val realSym = realTpe.typeSymbol
    val rawSym = rawTpe.typeSymbol

    if !realSym.flags.is(Flags.Trait) && !realSym.flags.is(Flags.Abstract) then
      report.errorAndAbort(s"Real type ${realTpe.show} must be a trait or abstract class.")

    val realMethods = realSym.declaredMethods.filterNot(m =>
      m.flags.is(Flags.Synthetic) || m.flags.is(Flags.Private) || m.flags.is(Flags.PrivateLocal),
    )
    val rawMethods = rawSym.declaredMethods.filterNot(m =>
      m.flags.is(Flags.Synthetic) || m.flags.is(Flags.Private) || m.flags.is(Flags.PrivateLocal),
    )

    // Each Real method paired with a Raw method by name. Tier 2 will add annotation-driven matching.
    val pairs: List[(Symbol, Symbol)] = realMethods.map { realMember =>
      rawMethods.find(_.name == realMember.name) match
        case Some(rawMember) => realMember -> rawMember
        case None => report.errorAndAbort(
            s"No matching Raw method for Real method '${realMember.name}'. " +
              s"Tier-1 RPC derivation requires identical method names on Raw and Real. " +
              s"Real: ${realTpe.show}, Raw: ${rawTpe.show}.",
          )
    }

    // Validate single param list with no implicit/type params (Tier-1 restriction).
    pairs.foreach { case (realMember, _) =>
      if realMember.paramSymss.exists(_.exists(_.isTypeParam)) then
        report.errorAndAbort(
          s"Real method '${realMember.name}' has type parameters; not supported in Tier-1.",
        )
      if realMember.paramSymss.size > 1 then
        report.errorAndAbort(
          s"Real method '${realMember.name}' has multiple parameter lists; not supported in Tier-1.",
        )
    }

    val proxyBaseTpe = TypeRepr.of[RpcProxyBase[Real]]

    val clsSym = Symbol.newClass(
      Symbol.spliceOwner,
      "RpcRawProxy$",
      parents = List(proxyBaseTpe, rawTpe),
      decls = cls => pairs.map { case (_, rawMember) =>
        val rawMethodType = rawTpe.memberType(rawMember).widen
        Symbol.newMethod(cls, rawMember.name, rawMethodType, Flags.Override, Symbol.noSymbol)
      },
      selfType = None,
    )

    val methodDefs = pairs.map { case (realMember, rawMember) =>
      val proxyMethodSym = clsSym.declaredMethod(rawMember.name).head
      DefDef(
        proxyMethodSym,
        paramss => {
          // Extract Term args from the first param list (Tier-1 single-list invariant).
          val rawTermArgs: List[Term] = paramss.flatten.collect { case t: Term => t }
          // Real param types and Raw param types.
          val realMethodType = realTpe.memberType(realMember).widen
          val rawMethodType = rawTpe.memberType(rawMember).widen
          val (realParamTpes, realRetTpe) = realMethodType match
            case MethodType(_, ps, r) => (ps, r)
            case t => (Nil, t)
          val (rawParamTpes, rawRetTpe) = rawMethodType match
            case MethodType(_, ps, r) => (ps, r)
            case t => (Nil, t)

          if realParamTpes.size != rawParamTpes.size then
            report.errorAndAbort(
              s"Arity mismatch on '${realMember.name}': Real has ${realParamTpes.size} params, " +
                s"Raw has ${rawParamTpes.size}.",
              proxyMethodSym.pos.getOrElse(Position.ofMacroExpansion),
            )

          // Decode each Raw arg into the corresponding Real arg via AsReal[RawArg, RealArg].
          val realArgTerms: List[Term] = (realParamTpes lazyZip rawParamTpes lazyZip rawTermArgs).toList.map {
            case (realPT, rawPT, rawArg) =>
              (realPT.asType: @scala.annotation.nowarn("msg=exhaustive"), rawPT.asType) match
                case ('[rp], '[rwp]) =>
                  val asRealExpr = Expr.summon[AsReal[rwp, rp]].getOrElse(
                    report.errorAndAbort(
                      s"Cannot find AsReal[${rawPT.show}, ${realPT.show}] for parameter of '${realMember.name}'.",
                    ),
                  )
                  '{ $asRealExpr.asReal(${ rawArg.asExprOf[rwp] }) }.asTerm
                case _ => report.errorAndAbort(s"Unexpected type shape on param of '${realMember.name}'")
          }

          // Read `this.real` via the inherited RpcProxyBase[Real] field.
          val selfExpr = This(clsSym).asExprOf[RpcProxyBase[Real]]
          val realRef = '{ $selfExpr.real }.asTerm
          val callReal = realRef.select(realMember).appliedToArgs(realArgTerms)

          // Encode the Real return value into Raw via AsRaw[RawRet, RealRet].
          val bodyTerm: Term = (realRetTpe.asType, rawRetTpe.asType) match
            case ('[realR], '[rawR]) =>
              if realRetTpe =:= TypeRepr.of[Unit] then callReal
              else
                val asRawExpr = Expr.summon[AsRaw[rawR, realR]].getOrElse(
                  report.errorAndAbort(
                    s"Cannot find AsRaw[${rawRetTpe.show}, ${realRetTpe.show}] for return of '${realMember.name}'.",
                  ),
                )
                '{ $asRawExpr.asRaw(${ callReal.asExprOf[realR] }) }.asTerm
            case _ => report.errorAndAbort(s"Unexpected return type shape for '${realMember.name}'")

          Some(bodyTerm)
        },
      )
    }

    // Pass the `real` parameter into RpcProxyBase[Real]'s constructor inside `asRaw(real)`.
    val instance = '{
      new AsRaw[Raw, Real]:
        def asRaw(real: Real): Raw = ${
          val baseTypeTree = TypeTree.of[RpcProxyBase[Real]]
          val baseCtor = proxyBaseTpe.typeSymbol.primaryConstructor
          val baseAppliedTypes: Term =
            New(baseTypeTree).select(baseCtor).appliedToType(realTpe)
          val baseCall: Term =
            baseAppliedTypes.appliedTo('{ real }.asTerm)
          val clsDef = ClassDef(clsSym, List(baseCall, TypeTree.of[Raw]), methodDefs)
          val newInstance = Typed(
            Apply(Select(New(TypeIdent(clsSym)), clsSym.primaryConstructor), Nil),
            TypeTree.of[Raw],
          )
          Block(List(clsDef), newInstance).asExprOf[Raw]
        }
    }
    instance

  private def materializeAsRealImpl[Raw: Type, Real: Type](using Quotes): Expr[AsReal[Raw, Real]] =
    import quotes.reflect.*
    report.errorAndAbort(
      s"RpcDerivation.materializeAsReal is not yet implemented. " +
        s"Tier-1 currently covers AsRaw only; AsReal requires generating a Real instance " +
        s"backed by a Raw — coming in a follow-up commit.",
    )
