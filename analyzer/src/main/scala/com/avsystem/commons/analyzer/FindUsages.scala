package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*

class FindUsages() extends CheckingRule("findUsages"):
  private def parseRejectedSymbols: Set[String] =
    if ruleArgument == null then Set.empty
    else ruleArgument.nn.split(";").toSet

  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    val rejectedSet = parseRejectedSymbols
    if rejectedSet.isEmpty then return

    object UsageFinder extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        if tree.symbol.exists then
          val fullName = tree.symbol.fullName.toString
          if rejectedSet.contains(fullName) then
            emitReport(tree.srcPos, s"found usage of $fullName")
        traverseChildren(tree)
    UsageFinder.traverse(unitTree)
