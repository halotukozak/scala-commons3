package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

class ImplicitFunctionParams(using Context) extends AnalyzerRuleOnTyped("implicitFunctionParams", Level.Warn) {
  def analyze(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case dd: DefDef if dd.symbol.is(Flags.Method) =>
      for {
        paramClause <- dd.termParamss
        if paramClause.nonEmpty && paramClause.head.symbol.is(Flags.Implicit)
        param <- paramClause
        paramType = param.tpt.tpe
        if paramType != null && (defn.isFunctionType(paramType) || paramType.typeSymbol == defn.PartialFunctionClass)
      } emitReport(param.srcPos, "Implicit parameters should not have any function type")
    case _ =>
  }
}
