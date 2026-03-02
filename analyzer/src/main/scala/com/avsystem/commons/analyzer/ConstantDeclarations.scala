package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Types.ConstantType

class ConstantDeclarations extends AnalyzerRule("constantDeclarations", level = Level.Off) {

  override def verifyValDef(tree: tpd.ValDef)(using Context): Unit = {
    val sym = tree.symbol

    // Skip local vals, params, modules (objects), synthetic (compiler-generated)
    if (
      !sym.exists || !sym.owner.isClass || !sym.isPublic ||
      sym.isOneOf(Flags.Module | Flags.Param | Flags.Synthetic | Flags.Mutable | Flags.Param)
    ) return

    val isConstantValue = tree.rhs.tpe.widenTermRefExpr.isInstanceOf[ConstantType]
    lazy val isUpperCase = sym.name.toString.charAt(0).isUpper
    lazy val isFinal = sym.is(Flags.Final)

    if (isConstantValue && (!isUpperCase || !isFinal)) {
      // Case 1: Literal-valued constant with lowercase name OR not final
      report(
        tree,
        "a literal-valued constant should be declared as a `final val` with an UpperCamelCase name and no explicit type annotation",
      )
    } else if (!isConstantValue && isUpperCase && !isFinal) {
      // Case 2: Non-constant UpperCamelCase val without final
      report(
        tree,
        "an UpperCamelCase val should be declared as a `final val`",
      )
    } else if (isFinal && isConstantValue) {
      // Case 3: Final literal-valued constant with explicit type annotation
      val tpt = tree.tpt
      val hasExplicitType = tpt.span.exists && tpt.span.isSourceDerived && tpt.span != tree.nameSpan &&
        tpt.span.start != tpt.span.end

      if (hasExplicitType) {
        report(
          tree,
          "a constant with a literal value should not have an explicit type annotation (to enable constant inlining)",
        )
      }
    }
  }
}
