package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*

class CheckMacroPrivate() extends AnalyzerRule("macroPrivate") {
  private def extractMacroPrivateAnnotation(using Context): Type =
    resolveClassType("com.avsystem.commons.annotation.macroPrivate")

  def performCheck(unitTree: Tree)(using Context): Unit = {
    val macroPrivateType = extractMacroPrivateAnnotation
    if (macroPrivateType != NoType) {
      checkChildren(unitTree) { tree =>
        tree match {
          case ref: RefTree if ref.symbol.exists && ref.span.exists =>
            val sym = ref.symbol
            val hasAnnotation = (sym :: sym.allOverriddenSymbols.toList).exists { s =>
              s.annotations.exists(_.symbol.typeRef <:< macroPrivateType)
            }
            if (hasAnnotation) {
              emitReport(ref.srcPos, s"$sym can only be used in macro-generated code")
            }
          case _ =>
        }
      }
    }
  }
}
