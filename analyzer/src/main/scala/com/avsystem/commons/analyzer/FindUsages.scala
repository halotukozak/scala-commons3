package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*

class FindUsages(using Context) extends AnalyzerRuleOnTyped("findUsages") {
  private lazy val parseRejectedSymbols = Option(ruleArgument).map(_.split(";").toSet)

  def analyze(unitTree: Tree)(using Context): Unit = parseRejectedSymbols.foreach { rejectedSet =>
    checkChildren(unitTree) {
      case tree if tree.symbol.exists =>
        val fullName = tree.symbol.fullName.toString
        if (rejectedSet.contains(fullName))
          emitReport(tree.srcPos, s"found usage of $fullName")
      case _ =>
    }
  }
}
