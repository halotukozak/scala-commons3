package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*

class FinalValueClasses(using Context) extends AnalyzerRuleOnTyped("finalValueClasses", Level.Warn) {
  def analyze(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case classDef: TypeDef if classDef.symbol.isClass && !classDef.symbol.is(Flags.Final) =>
      val classType = classDef.symbol.info
      if (classType.baseClasses.contains(defn.AnyValClass))
        emitReport(classDef.srcPos, "Value classes should be marked as final")
    case _ =>
  }
}
