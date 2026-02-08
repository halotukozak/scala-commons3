package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

class FindUsages(using Context) extends AnalyzerRuleOnTyped("findUsages") {
  private lazy val rejectedSymbols = Option(argument).map(_.split(";").toSet)

  def analyze(unitTree: Tree)(using Context): Unit = rejectedSymbols.foreach { rejectedSet =>
    checkChildren(unitTree) {
      case tree if tree.symbol.exists =>
        if (rejectedSet.contains(tree.symbol.fullName.toString)) {
          emitReport(tree.srcPos, s"found usage of ${tree.symbol.fullName.toString}")
        }
      case _ =>
    }
  }
}
