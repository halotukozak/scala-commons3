package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.Type

class ImplicitFunctionParams extends AnalyzerRule("implicitFunctionParams") {
  override def verifyDefDef(tree: tpd.DefDef)(using Context): Unit = for {
    paramList <- tree.termParamss
    param <- paramList
    if param.symbol.isOneOf(Flags.GivenOrImplicit) && isFunctionLikeType(param.tpt.tpe)
  } {
    report(
      param,
      "implicit/using parameter should not be a function type; consider a non-implicit parameter or a type class instead",
    )
  }

  private def isFunctionLikeType(tpe: Type)(using Context): Boolean =
    defn.isFunctionType(tpe) || defn.isContextFunctionType(tpe) || tpe.isRef(defn.PartialFunctionClass)
}
