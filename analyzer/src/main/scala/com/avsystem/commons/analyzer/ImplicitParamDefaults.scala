package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*

class ImplicitParamDefaults(using Context) extends AnalyzerRuleOnTyped("implicitParamDefaults", Level.Warn) {
  def analyze(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case defDef: DefDef if defDef.symbol.is(Flags.Method) =>
      defDef.termParamss
        .filter(paramClause => paramClause.nonEmpty && paramClause.head.symbol.is(Flags.Implicit))
        .foreach { paramClause =>
          paramClause.filter(param => !param.rhs.isEmpty).foreach { param =>
            emitReport(param.srcPos, "Implicit parameters should not have default values")
          }
        }
    case _ =>
  }
}
