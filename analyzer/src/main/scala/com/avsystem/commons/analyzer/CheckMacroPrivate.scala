package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

class CheckMacroPrivate(using Context) extends AnalyzerRuleOnTyped("macroPrivate") {
  private lazy val extractMacroPrivateAnnotation = resolveClassType("com.avsystem.commons.annotation.macroPrivate")

  def analyze(unitTree: Tree)(using Context): Unit = extractMacroPrivateAnnotation.foreach { macroPrivateType =>
    checkChildren(unitTree) {
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
