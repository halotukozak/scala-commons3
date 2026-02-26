package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class ShowAst(using Context) extends AnalyzerRule("showAst", level = Level.Error) {
  private lazy val showAstAnnotClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.annotation.showAst")

  override def requiredSymbols: List[Symbol] = showAstAnnotClass :: Nil

  override def verifyValDef(tree: tpd.ValDef)(using Context): Unit =
    checkShowAst(tree)

  override def verifyDefDef(tree: tpd.DefDef)(using Context): Unit =
    checkShowAst(tree)

  override def verifyTypeDef(tree: tpd.TypeDef)(using Context): Unit =
    checkShowAst(tree)

  private def checkShowAst(tree: tpd.MemberDef)(using Context): Unit =
    if (tree.symbol != NoSymbol && tree.symbol.hasAnnotation(showAstAnnotClass)) {
      report(tree, tree.show)
    }
}
