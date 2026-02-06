package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*

class CheckBincompat(using Context) extends AnalyzerRuleOnTyped("bincompat") {
  private lazy val extractBincompatAnnotation = resolveClassType("com.avsystem.commons.annotation.bincompat")

  def performCheck(unitTree: Tree)(using Context): Unit = extractBincompatAnnotation.foreach { bincompatType =>
    checkChildren(unitTree) {
      case ref: RefTree if ref.symbol.exists && ref.symbol.annotations.exists(_.symbol.typeRef <:< bincompatType) =>
        emitReport(
          ref.srcPos,
          "Symbols annotated as @bincompat exist only for binary compatibility " + "and should not be used directly",
        )
      case _ =>
    }
  }
}
