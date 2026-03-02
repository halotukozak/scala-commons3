package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

class ImplicitParamDefaults extends AnalyzerRule("implicitParamDefaults") {
  override def verifyDefDef(tree: tpd.DefDef)(using Context): Unit = for {
    (paramList, idx) <- tree.termParamss.zipWithIndex
    param <- paramList
    if param.symbol.isOneOf(Flags.GivenOrImplicit) && param.symbol.is(Flags.HasDefault)
  } {
    report(param, "Implicit parameters should not have default values")
  }
}
