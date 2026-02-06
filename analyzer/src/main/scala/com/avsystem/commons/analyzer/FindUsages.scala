package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*

class FindUsages() extends AnalyzerRule("findUsages") {
  private def parseRejectedSymbols: Set[String] =
    if (ruleArgument == null) Set.empty
    else ruleArgument.nn.split(";").toSet

  def performCheck(unitTree: Tree)(using Context): Unit = {
    val rejectedSet = parseRejectedSymbols
    if (rejectedSet.nonEmpty) {
      checkChildren(unitTree) { tree =>
        if (tree.symbol.exists) {
          val fullName = tree.symbol.fullName.toString
          if (rejectedSet.contains(fullName)) emitReport(tree.srcPos, s"found usage of $fullName")
        }
      }
    }
  }
}
