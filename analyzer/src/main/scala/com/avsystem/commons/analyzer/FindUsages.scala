package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

class FindUsages extends AnalyzerRule("findUsages") {
  private lazy val rejectedSymbols: Set[String] =
    argument.map(_.split(";").toSet).getOrElse(Set.empty)

  override def verifyIdent(tree: tpd.Ident)(using Context): Unit = checkTree(tree)

  override def verifySelect(tree: tpd.Select)(using Context): Unit = checkTree(tree)

  override def verifyApply(tree: tpd.Apply)(using Context): Unit = checkTree(tree)

  override def verifyNew(tree: tpd.New)(using Context): Unit = checkTree(tree)

  override def verifyOther(tree: tpd.Tree)(using Context): Unit = checkTree(tree)

  private def checkTree(tree: tpd.Tree)(using Context): Unit =
    if (rejectedSymbols.nonEmpty && tree.symbol != NoSymbol) {
      val fullName = tree.symbol.fullName.toString
      if (rejectedSymbols.contains(fullName)) {
        report(tree, s"found usage of $fullName")
      }
    }
}
