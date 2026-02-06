package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

class FinalValueClasses(using Context) extends AnalyzerRuleOnTyped("finalValueClasses", Level.Warn) {
  def analyze(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case cd: TypeDef if cd.symbol.isClass && !cd.symbol.is(Flags.Final) =>
      val tpe = cd.symbol.info
      if (tpe.baseClasses.contains(defn.AnyValClass)) {
        emitReport(cd.srcPos, "Value classes should be marked as final")
      }
    case _ =>
  }
}
