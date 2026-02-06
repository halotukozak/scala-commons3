package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*

class ImplicitFunctionParams(using Context) extends AnalyzerRuleOnTyped("implicitFunctionParams", Level.Warn) {
  def performCheck(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case defDef: DefDef if defDef.symbol.is(Flags.Method) =>
      defDef.termParamss.foreach { paramClause =>
        if (paramClause.nonEmpty && paramClause.head.symbol.is(Flags.Implicit))
          paramClause.foreach { param =>
            val paramType = param.tpt.tpe
            if (
              paramType != null && (defn.isFunctionType(paramType) || paramType.typeSymbol == defn.PartialFunctionClass)
            )
              emitReport(param.srcPos, "Implicit parameters should not have any function type")
          }
      }
    case _ =>
  }
}
