package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*
import printing.Texts.Text

class ShowAst() extends CheckingRule("showAst", SeverityLevel.Fatal):
  private def extractShowAstAnnotation(using Context): Type =
    resolveClassType("com.avsystem.commons.annotation.showAst")

  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    val showAstType = extractShowAstAnnotation
    if showAstType == NoType then return

    object AstShower extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case tpd.Annotated(arg, annot) if annot.symbol.typeRef <:< showAstType =>
            emitReport(arg.srcPos, arg.show)
          case tpd.Typed(expr, tpt) if tpt.tpe.typeSymbol.annotations.exists(_.symbol.typeRef <:< showAstType) =>
            emitReport(expr.srcPos, expr.show)
          case defTree: tpd.MemberDef if defTree.symbol.annotations.exists(_.symbol.typeRef <:< showAstType) =>
            emitReport(defTree.srcPos, defTree.show)
          case _ =>
        traverseChildren(tree)
    AstShower.traverse(unitTree)
