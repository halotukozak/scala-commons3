package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

class ImplicitParamDefaults(using Context) extends AnalyzerRuleOnTyped("implicitParamDefaults", Level.Warn) {
  def analyze(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case dd: DefDef if dd.symbol.is(Flags.Method) =>
      for {
        paramClause <- dd.termParamss
        if paramClause.nonEmpty && paramClause.head.symbol.is(Flags.Implicit)
        param <- paramClause
        if !param.rhs.isEmpty
      } emitReport(param.srcPos, "Implicit parameters should not have default values")
    case _ =>
  }
}
