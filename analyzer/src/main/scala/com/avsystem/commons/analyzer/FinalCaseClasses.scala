package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*

class FinalCaseClasses() extends AnalyzerRule("finalCaseClasses", Level.Warn) {
  def performCheck(unitTree: Tree)(using Context): Unit = {
    checkChildren(unitTree) { tree =>
      tree match {
        case classDef: TypeDef
            if classDef.symbol.isClass && classDef.symbol.is(Flags.Case) && !classDef.symbol.is(Flags.Final) &&
              !classDef.symbol.is(Flags.Sealed) =>

          val isTopLevelOrStatic = classDef.symbol.isStatic

          if (isTopLevelOrStatic)
            emitReport(classDef.srcPos, "Case classes should be marked as final")
          else
            emitReport(
              classDef.srcPos,
              "Case classes should be marked as final. Due to the SI-4440 bug, it cannot be done here. Consider moving the case class to the companion object",
              severity = Level.Info,
            )
        case _ =>
      }
    }
  }
}
