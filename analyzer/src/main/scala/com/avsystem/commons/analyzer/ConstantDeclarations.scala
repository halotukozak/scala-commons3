package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*

class ConstantDeclarations(using Context) extends AnalyzerRuleOnTyped("constantDeclarations", Level.Off) {
  def analyze(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case t @ ValDef(name, tpt, _) if t.symbol.exists && t.symbol.owner.isEffectivelyFinal =>
      val valSym = t.symbol
      if (valSym.isPublic && !valSym.allOverriddenSymbols.iterator.hasNext) {
        val isLiteralConstant = t.rhs.tpe match {
          case ConstantType(_) => true
          case _ => false
        }

        val valName = name.toString
        val startsUpper = valName.nonEmpty && valName.charAt(0).isUpper
        val isFinalVal = valSym.is(Flags.Final)

        def emitDiagnostic(msg: String): Unit =
          emitReport(t.srcPos, msg)

        if (isLiteralConstant && (!startsUpper || !isFinalVal)) {
          emitDiagnostic(
            "a literal-valued constant should be declared as a `final val` with an UpperCamelCase name",
          )
        }

        if (!isLiteralConstant && startsUpper && !isFinalVal) {
          emitDiagnostic("a constant with UpperCamelCase name should be declared as a `final val`")
        }

        if (isFinalVal && isLiteralConstant && tpt.tpe != null && !(tpt.tpe =:= t.rhs.tpe)) {
          emitDiagnostic("a constant with a literal value should not have an explicit type annotation")
        }
      }
    case _ =>
  }
}
