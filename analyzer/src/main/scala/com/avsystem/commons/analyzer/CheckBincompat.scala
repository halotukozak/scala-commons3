package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*

class CheckBincompat() extends AnalyzerRule("bincompat"):
  private def extractBincompatAnnotation(using Context): Type =
    resolveClassType("com.avsystem.commons.annotation.bincompat")

  def performCheck(unitTree: Tree)(using Context): Unit =
    val bincompatType = extractBincompatAnnotation
    if bincompatType != NoType then
      checkChildren(unitTree) { tree =>
        tree match
          case ref: RefTree if ref.symbol.exists && ref.symbol.annotations.exists(_.symbol.typeRef <:< bincompatType) =>
            emitReport(
              ref.srcPos,
              "Symbols annotated as @bincompat exist only for binary compatibility " +
                "and should not be used directly"
            )
          case _ =>
      }
