package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*
import Names.*

class Any2StringAdd() extends CheckingRule("any2stringadd", SeverityLevel.Disabled):
  private def findAny2StringAddSymbol(using Context): Symbol =
    val predefModule = requiredModule("scala.Predef")
    predefModule.info.decl(termName("any2stringadd")).symbol

  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    val any2stringaddSym = findAny2StringAddSymbol
    if !any2stringaddSym.exists then return

    object StringAddChecker extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        if tree.symbol == any2stringaddSym then
          emitReport(
            tree.srcPos,
            "concatenating arbitrary values with strings is disabled, " +
              "use explicit toString or string interpolation"
          )
        traverseChildren(tree)
    StringAddChecker.traverse(unitTree)
