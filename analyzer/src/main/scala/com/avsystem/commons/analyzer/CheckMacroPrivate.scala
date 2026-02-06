package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*

class CheckMacroPrivate() extends CheckingRule("macroPrivate") {
  private def extractMacroPrivateAnnotation(using Context): Type =
    resolveClassType("com.avsystem.commons.annotation.macroPrivate")

  def performCheck(unitTree: tpd.Tree)(using Context): Unit = {
    val macroPrivateType = extractMacroPrivateAnnotation
    if (macroPrivateType == NoType) return

    object MacroPrivateChecker extends tpd.TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match {
          case ref: tpd.RefTree if ref.symbol.exists && ref.span.exists =>
            val sym = ref.symbol
            val hasAnnotation = (sym :: sym.allOverriddenSymbols.toList).exists { s =>
              s.annotations.exists(_.symbol.typeRef <:< macroPrivateType)
            }

            if (hasAnnotation) {
              emitReport(ref.srcPos, s"$sym can only be used in macro-generated code")
            }
            traverseChildren(tree)
          case _ => traverseChildren(tree)
        }
    }
    MacroPrivateChecker.traverse(unitTree)
  }
}
