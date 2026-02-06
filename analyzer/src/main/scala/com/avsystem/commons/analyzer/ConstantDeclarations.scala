package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*
import Constants.*

class ConstantDeclarations(using Context) extends AnalyzerRuleOnTyped("constantDeclarations", Level.Off) {
  def performCheck(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case valDef: ValDef if valDef.symbol.exists && valDef.symbol.owner.isEffectivelyFinal =>
      val valSym = valDef.symbol
      if (valSym.isPublic && !valSym.allOverriddenSymbols.iterator.hasNext) {
        val isLiteralConstant = valDef.rhs.tpe match {
          case ConstantType(_) => true
          case _ => false
        }

        val valName = valDef.name.toString
        val startsUpper = valName.nonEmpty && valName.charAt(0).isUpper
        val isFinalVal = valSym.is(Flags.Final)

        def emitDiagnostic(msg: String): Unit =
          emitReport(valDef.srcPos, msg)

        if (isLiteralConstant && (!startsUpper || !isFinalVal)) {
          emitDiagnostic(
            "a literal-valued constant should be declared as a `final val` with an UpperCamelCase name",
          )
        }

        if (!isLiteralConstant && startsUpper && !isFinalVal) {
          emitDiagnostic("a constant with UpperCamelCase name should be declared as a `final val`")
        }

        if (isFinalVal && isLiteralConstant && valDef.tpt.tpe != null && !(valDef.tpt.tpe =:= valDef.rhs.tpe)) {
          emitDiagnostic("a constant with a literal value should not have an explicit type annotation")
        }
      }
    case _ =>
  }
}
