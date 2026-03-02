package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.termName

/**
 * Warns when implicit val/def or named given definitions have inferred
 * (missing) type annotations. Explicit type annotations on implicit/given
 * definitions improve code clarity and prevent accidental type widening.
 *
 * Does NOT warn on:
 *  - anonymous givens (their type is inherently declared)
 *  - non-implicit/non-given definitions
 *  - compiler-generated (Synthetic) definitions
 *  - parameters (they have different semantics)
 */
class ImplicitTypes extends AnalyzerRule("implicitTypes") {

  override def verifyValDef(tree: tpd.ValDef)(using Context): Unit =
    checkImplicitType(tree, tree.tpt)

  override def verifyDefDef(tree: tpd.DefDef)(using Context): Unit =
    checkImplicitType(tree, tree.tpt)

  private def checkImplicitType(tree: tpd.MemberDef, tpt: tpd.Tree)(using Context): Unit = {
    val sym = tree.symbol
    if (sym.isOneOf(Flags.GivenOrImplicit, butNot = Flags.Synthetic | Flags.Param)) {
      // Skip generated implicit conversion methods for implicit classes.
      // The compiler generates an implicit def with the same name as the class.
      val isImplicitClassConversion = sym.is(Flags.Method) && {
        val classSym = sym.owner.info.member(sym.name.toTypeName).symbol
        classSym.isClass && classSym.is(Flags.Implicit)
      }
      if (!isImplicitClassConversion) {
        // Detect inferred type: when the compiler infers a type, the TypeTree's span
        // is either non-existent or not source-derived (synthetic span).
        val typeInferred = !tpt.span.exists || !tpt.span.isSourceDerived
        if (typeInferred) {
          report(tree, "implicit/given definitions should have an explicit type annotation")
        }
      }
    }
  }
}
