package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

class FinalCaseClasses extends AnalyzerRule("finalCaseClasses") {
  override def verifyTypeDef(tree: tpd.TypeDef)(using Context): Unit =
    if (tree.isClassDef && tree.symbol.flags.is(Flags.Case, butNot = Flags.FinalOrSealed)) {
      report(tree, "Case classes should be marked as final")
    }
}
