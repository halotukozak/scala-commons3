package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*

class ImplicitParamDefaults() extends CheckingRule("implicitParamDefaults", SeverityLevel.Warning) {
  def performCheck(unitTree: tpd.Tree)(using Context): Unit = {
    object DefaultParamChecker extends tpd.TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match {
          case defDef: tpd.DefDef if defDef.symbol.is(Flags.Method) =>
            defDef.termParamss.foreach { paramClause =>
              if (paramClause.nonEmpty && paramClause.head.symbol.is(Flags.Implicit)) {
                paramClause.foreach { param =>
                  if (!param.rhs.isEmpty) {
                    emitReport(param.srcPos, "Implicit parameters should not have default values")
                  }
                }
              }
            }
            traverseChildren(tree)
          case _ => traverseChildren(tree)
        }
    }
    DefaultParamChecker.traverse(unitTree)
  }
}
