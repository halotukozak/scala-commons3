package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*

class CheckBincompat() extends CheckingRule("bincompat"):
  private def extractBincompatAnnotation(using Context): Type =
    resolveClassType("com.avsystem.commons.annotation.bincompat")

  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    val bincompatType = extractBincompatAnnotation
    if bincompatType == NoType then return

    object BincompatChecker extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case ref: tpd.RefTree if ref.symbol.exists && ref.symbol.annotations.exists(_.symbol.typeRef <:< bincompatType) =>
            emitReport(
              ref.srcPos,
              "Symbols annotated as @bincompat exist only for binary compatibility " +
                "and should not be used directly"
            )
            traverseChildren(tree)
          case _ => traverseChildren(tree)
    BincompatChecker.traverse(unitTree)
