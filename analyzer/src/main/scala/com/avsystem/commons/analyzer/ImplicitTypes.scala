package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*

class ImplicitTypes(using Context) extends AnalyzerRuleOnTyped("implicitTypes") {
  def performCheck(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case valOrDefDef: ValOrDefDef if valOrDefDef.symbol.is(Flags.Implicit) && !valOrDefDef.symbol.is(Flags.Synthetic) =>
      valOrDefDef.tpt match {
        case typeTree: TypeTree if !typeTree.span.exists || typeTree.span.isZeroExtent =>
          emitReport(valOrDefDef.srcPos, "Implicit definitions must have type annotated explicitly")
        case _ =>
      }
    case _ =>
  }
}
