package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*
import printing.Texts.Text

object ShowAst extends AnalyzerRule("showAst", Level.Error) {
  private def extractShowAstAnnotation(using Context) =
    resolveClassType("com.avsystem.commons.annotation.showAst")

  def performCheck(unitTree: Tree)(using Context): Unit = extractShowAstAnnotation.foreach { showAstType =>
    checkChildren(unitTree) {
      case Annotated(arg, annot) if annot.symbol.typeRef <:< showAstType =>
        emitReport(arg.srcPos, arg.show)
      case Typed(expr, tpt) if tpt.tpe.typeSymbol.annotations.exists(_.symbol.typeRef <:< showAstType) =>
        emitReport(expr.srcPos, expr.show)
      case defTree: MemberDef if defTree.symbol.annotations.exists(_.symbol.typeRef <:< showAstType) =>
        emitReport(defTree.srcPos, defTree.show)
      case _ =>
    }
  }
}
