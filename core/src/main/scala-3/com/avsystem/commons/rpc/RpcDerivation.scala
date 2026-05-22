package com.avsystem.commons
package rpc

import made.{Done, DoneOperation}

import scala.quoted.*

/**
 * Macro-driven derivation of RPC type-classes (`AsRaw`, `AsReal`) using `made.Done` as a
 * compile-time mirror of the Real RPC trait.
 *
 * **Tier 1 (current)** — "echo proxy": Raw and Real are traits with matching method
 * signatures (same names, same parameter counts, AsRaw/AsReal available pairwise for each
 * argument and the return type). `asRaw(real)` returns a Raw whose methods translate args
 * via `AsRaw[RawArg, RealArg]` and translate returns via `AsReal[RawRet, RealRet]`.
 *
 * Out of scope for Tier 1 (reported via `compiletime.error` or `report.errorAndAbort`):
 *   - `@multi` / `@verbatim` / `@encoded` / `@composite` / `@methodName` / `@annotated`
 *   - overloaded methods (use `@rpcName` to disambiguate)
 *   - method type parameters (`@forTypeParams`, `@infer`)
 *   - implicit parameter lists
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

    val realOps = realSym.declaredMethods.filter(m => !m.flags.is(Flags.Synthetic) && !m.flags.is(Flags.Private))
    val rawOps = rawSym.declaredMethods.filter(m => !m.flags.is(Flags.Synthetic) && !m.flags.is(Flags.Private))

    // Pair each Real method with a Raw method by name. Bail loudly if any Real method has no
    // matching Raw method — Tier 2 (annotation-driven mapping) will handle name divergence.
    val pairs: List[(Symbol, Symbol)] = realOps.map { realMember =>
      val name = realMember.name
      rawOps.find(_.name == name) match
        case Some(rawMember) => realMember -> rawMember
        case None =>
          report.errorAndAbort(
            s"No matching Raw method for Real method '$name'. Tier-1 RPC derivation requires " +
              s"Raw and Real to have identical method names. " +
              s"Real type: ${realTpe.show}, Raw type: ${rawTpe.show}.",
          )
    }

    // Build the proxy body via Symbol.newClass + DefDef overrides.
    val proxyParents = List(TypeTree.of[Object], TypeTree.of[Raw])
    val realTermName = "real"

    val proxySym = Symbol.newClass(
      Symbol.spliceOwner,
      "RawProxy",
      parents = proxyParents.map(_.tpe),
      decls = clsSym =>
        // val real: Real (constructor-injected, modeled as private val field)
        Symbol.newVal(clsSym, realTermName, realTpe, Flags.Private, Symbol.noSymbol) ::
          pairs.map { case (realMember, rawMember) =>
            val rawMethodType = rawTpe.memberType(rawMember).widen
            Symbol.newMethod(clsSym, rawMember.name, rawMethodType, Flags.Override, Symbol.noSymbol)
          },
      selfType = None,
    )

    val realFieldSym = proxySym.declaredField(realTermName)

    def proxyMethodBody(self: Term, realMember: Symbol, rawMember: Symbol)(args: List[List[Term]]): Term =
      // Tier-1 echo proxy: args translated via AsReal[RawArg, RealArg], return via AsRaw[RawRet, RealRet].
      // Implementation deferred — emit `???` for now so the class compiles. Tier-1 finalization
      // will replace this with real arg/return wiring once the per-symbol AsRaw/AsReal lookups
      // are implemented (next commit).
      '{ ??? }.asTerm

    val proxyMethodDefs = pairs.map { case (realMember, rawMember) =>
      val rawProxySym = proxySym.declaredMethod(rawMember.name).head
      DefDef(rawProxySym, paramss => Some(proxyMethodBody(This(proxySym), realMember, rawMember)(paramss.collect {
        case ts: List[Term @unchecked] => ts
      })))
    }

    val realValDef = ValDef(realFieldSym, Some('{ ??? }.asTerm))

    val proxyClassDef = ClassDef(proxySym, proxyParents, realValDef :: proxyMethodDefs)

    val instance = '{
      new AsRaw[Raw, Real]:
        def asRaw(real: Real): Raw = ${
          val proxyInstance = Typed(
            Apply(Select(New(TypeIdent(proxySym)), proxySym.primaryConstructor), Nil),
            TypeTree.of[Raw],
          )
          Block(List(proxyClassDef), proxyInstance).asExprOf[Raw]
        }
    }
    instance

  private def materializeAsRealImpl[Raw: Type, Real: Type](using Quotes): Expr[AsReal[Raw, Real]] =
    import quotes.reflect.*
    report.errorAndAbort(
      s"RpcDerivation.materializeAsReal is not yet implemented. " +
        s"Use scala-2 macros for now or stub a manual AsReal instance.",
    )
